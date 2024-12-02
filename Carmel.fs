namespace CarmelNet

open System
open System.Net.Http

[<RequireQualifiedAccess>]
type CarmelEnvironment =
    | Sandbox
    | Production

    member this.Uri() =
        match this with
        | Sandbox -> "https://api-sandbox.carmelsolutions.com"
        | Production -> "https://api.carmelsolutions.com"

[<RequireQualifiedAccess>]
type CarmelAccessToken =
    | Token of string

    override this.ToString() =
        match this with
        | Token x -> x


/// Direction: Credit (money to customer) or Debit (money from customer)
[<RequireQualifiedAccess>]
type CarmelTransferDirection =
    /// If you want to transfer money from your origination account to someone else's account this is known as a credit.
    | Credit
    /// if you want to transfer money from someone else's account to your origination account this is known as a debit.
    | Debit

    override this.ToString() =
        match this with
        | Credit -> "credit"
        | Debit -> "debit"

/// SubTransfer type for ACH: SEC-CODE: https://docs.carmelsolutions.com/reference/sec-codes
[<RequireQualifiedAccess>]
type CarmelSecCode =
    /// Internet Initiated Payment or Mobile Initiated Payment entry
    | WEB
    /// Telephone initiated entry
    | TEL
    /// Prearranged Payment and Deposit entry
    | PPD
    /// Corporate Credit or Debit
    | CCD

    override this.ToString() =
        match this with
        | PPD -> "PPD"
        | TEL -> "TEL"
        | WEB -> "WEB"
        | CCD -> "CCD"

/// Type: ACH transfer or Wire transfer
[<RequireQualifiedAccess>]
type CarmelTransferType =
    | ACH of CarmelSecCode
    | Wire

    override this.ToString() =
        match this with
        | ACH _ -> "ACH"
        | Wire -> "Wire"

/// Type: Target bank account type
[<RequireQualifiedAccess>]
type CarmelBankAccountType =
    | Checking
    | Savings

    override this.ToString() =
        match this with
        | Checking -> "checking"
        | Savings -> "savings"

[<RequireQualifiedAccess>]
type CarmelApprovalAction =
    /// Approve a payment order
    | Approve
    /// Cancel a payment order
    | Cancel

module internal Utils =

    open System.IO
    open System.Net

    let timeoutMs = 15000

    [<Struct>]
    type PostRequestTypes =
        | Text
        | ApplicationXml
        | ApplicationSoapXml
        | ApplicationJson
        | ApplicationJson_HTTP11
        | ApplicationUrlForm

    [<Struct; RequireQualifiedAccess>]
    type HttpVerb =
        | POST
        | GET
        | DELETE


    /// Make a post-web-request, with custom headers
    let makeVerbRequestWithHeadersAndTimeout
        (verb: HttpVerb)
        (reqType: PostRequestTypes)
        (url: string)
        (requestBody: string)
        (headers)
        =
        let req = WebRequest.CreateHttp url

        headers
        |> Seq.iter (fun (h: string, k: string) ->
            if not (String.IsNullOrEmpty h) then
                if h.ToLower() = "user-agent" then
                    req.UserAgent <- k
                else
                    req.Headers.Add(h, k))

        req.CookieContainer <- new CookieContainer()

        req.Method <-
            match verb with
            | HttpVerb.POST -> "POST"
            | HttpVerb.GET -> "GET"
            | HttpVerb.DELETE -> "DELETE"

        let timeout = timeoutMs // Timeout has to be smaller than DTC timeout
        req.Timeout <- timeout
        req.ProtocolVersion <- HttpVersion.Version10

        let postBytes =
            if requestBody <> null then
                let postBytesData = requestBody |> System.Text.Encoding.ASCII.GetBytes
                req.ContentLength <- postBytesData.LongLength
                postBytesData
            else
                Array.empty

        let reqtype =
            match reqType with
            | Text -> "text/xml; charset=utf-8"
            | ApplicationXml -> "application/xml; charset=utf-8"
            | ApplicationSoapXml -> "application/soap+xml; charset=utf-8"
            | ApplicationJson -> "application/json"
            | ApplicationJson_HTTP11 ->
                req.ProtocolVersion <- Version("1.1")
                "application/json"
            | ApplicationUrlForm -> "application/x-www-form-urlencoded"

        req.ContentType <- reqtype

        let asynccall =
            async {
                let! res =
                    async {
                        if not (String.IsNullOrEmpty requestBody) then
                            let! reqStream = req.GetRequestStreamAsync() |> Async.AwaitTask

                            do!
                                reqStream.WriteAsync(postBytes, 0, postBytes.Length)
                                |> Async.AwaitIAsyncResult
                                |> Async.Ignore

                            reqStream.Close()

                        let! res =
                            async { // Async methods are not using req.Timeout
                                let! child = Async.StartChild(req.AsyncGetResponse(), timeout)
                                return! child
                            }

                        use stream = res.GetResponseStream()
                        use reader = new StreamReader(stream)
                        let! rdata = reader.ReadToEndAsync() |> Async.AwaitTask
                        return rdata
                    }
                    |> Async.Catch

                match res with
                | Choice1Of2 x -> return x, None
                | Choice2Of2 e ->
                    match e with
                    | :? WebException as wex when not (isNull wex.Response) ->
                        use stream = wex.Response.GetResponseStream()
                        use reader = new StreamReader(stream)
                        let err = reader.ReadToEnd()
                        return err, Some e
                    | :? TimeoutException as e -> return failwith (e.ToString())
                    | _ ->
                        // System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(e).Throw()
                        return failwith (e.ToString())
            }

        asynccall
    //  |> Async.StartImmediateAsTask |> Task.WaitAll

    let makePostRequestWithHeadersAndTimeout (reqType: PostRequestTypes) (url: string) (requestBody: string) (headers) =
        makeVerbRequestWithHeadersAndTimeout HttpVerb.POST reqType url requestBody headers

    let makeGetRequestWithHeaders (reqType: PostRequestTypes) (url: string) (headers) =
        makeVerbRequestWithHeadersAndTimeout HttpVerb.GET reqType url "" headers


    let rec internal getErrorDetails: Exception -> string =
        function
        | :? AggregateException as aex -> getErrorDetails (aex.GetBaseException())
        | :? WebException as wex when not (isNull (wex.Response)) ->
            use stream = wex.Response.GetResponseStream()
            use reader = new System.IO.StreamReader(stream)
            let err = reader.ReadToEnd()
            err
        | :? TimeoutException as e -> "Timeout"
        | _ -> ""

    let mutable logUnsuccessfulHandler: Option<System.Net.HttpStatusCode * string -> Unit> =
        None

    let internal makeHttpClient (env: CarmelEnvironment) (access_token: CarmelAccessToken) =
        let httpClient =
            if logUnsuccessfulHandler.IsNone then
                new System.Net.Http.HttpClient(BaseAddress = Uri(env.Uri()))
            else
                let handler1 = new HttpClientHandler(UseCookies = false)
                let handler2 = new JsonData.ErrorHandler(handler1)
                new System.Net.Http.HttpClient(handler2, true, BaseAddress = Uri(env.Uri()))

        httpClient.DefaultRequestHeaders.Authorization <-
            Headers.AuthenticationHeaderValue("Bearer", access_token.ToString())

        httpClient.DefaultRequestHeaders.Add("User-Agent", "F# Client")
        httpClient

/// Documentation: https://docs.carmelsolutions.com/reference/introduction
module CarmelPayment =

    open Utils
    open FSharp.Data
    open FSharp.Data.JsonProvider

    let auth_server = "https://auth.carmelsolutions.com"
    let token_resource = auth_server + "/oauth2/token"

    let scope = "/pay"

    let getAcccessToken (env: CarmelEnvironment, clientId: string, clientSecret: string) =
        let carmelAuthHeader =
            [ "Authorization",
              "Basic "
              + ((clientId + ":" + clientSecret)
                 |> System.Text.Encoding.UTF8.GetBytes
                 |> Convert.ToBase64String)
              "Accept", "application/json" ]

        let authBody =
            $"""grant_type=client_credentials&scope={System.Web.HttpUtility.UrlEncode(env.Uri() + scope)}"""

        async {
            let! res, exn =
                Utils.makePostRequestWithHeadersAndTimeout ApplicationUrlForm token_resource authBody carmelAuthHeader

            match res, exn with
            | r, Some err -> return raise err
            | tokenResponse, None ->
                let isOk, token =
                    System.Text.Json.JsonSerializer
                        .Deserialize<System.Text.Json.JsonElement>(tokenResponse)
                        .TryGetProperty("access_token")

                if isOk then
                    return token.ToString() |> CarmelAccessToken.Token
                else
                    return failwith $"No access_token gotten {tokenResponse}"
        }

    let getOriginationAccount (env: CarmelEnvironment, access_token: CarmelAccessToken) =
        let httpClient = makeHttpClient env access_token
        let client = JsonData.CarmelOpenApi.Client httpClient

        async {

            let subscription =
                if logUnsuccessfulHandler.IsSome then
                    Some(JsonData.reportUnsuccessfulEvents logUnsuccessfulHandler.Value)
                else
                    None

            let! res = client.GetApiV1OriginationAccounts() |> Async.Catch
            httpClient.Dispose()

            if subscription.IsSome then
                subscription.Value.Dispose()

            match res with
            | Choice1Of2 x -> return Ok x.OriginationAccounts
            | Choice2Of2 err ->
                let details = getErrorDetails err

                //printfn "Used signature: %s" signature_bodyhash_string
                return Error(err, details)
        }


    //let getOriginationAccount(env:CarmelEnvironment, access_token:CarmelAccessToken) =
    //    let authHeader = ["Authorization", $"Bearer {access_token}"; "User-Agent", "F# Client"]
    //    async {

    //        let! res, exn = Utils.makeGetRequestWithHeaders ApplicationJson_HTTP11 (env.Uri() + "/api/v1/origination-accounts") authHeader
    //        match res, exn with
    //        | r, Some err -> return raise err
    //        | origination, None ->
    //            let oris = JsonData.OriginationAccountsResponse.Load (Serializer.Deserialize origination)
    //            return oris
    //    }

    //let createCreditTransfer (env:CarmelEnvironment, access_token:CarmelAccessToken, originationAccount:Guid, paymentId:string, amountCents:int, transferType:CarmelTransferType, direction:CarmelTransferDirection, routingNumber:string, accountNumber, accountHolderName, accountType:CarmelBankAccountType, address1:Option<string>) =

    //    match direction, transferType with
    //    | CarmelTransferDirection.Credit, CarmelTransferType.ACH CarmelSecCode.WEB
    //    | CarmelTransferDirection.Credit, CarmelTransferType.ACH CarmelSecCode.TEL -> failwith "Invalid transfer type. ACH Credit has to be PPD or CCD"
    //    | CarmelTransferDirection.Debit, CarmelTransferType.Wire -> failwith "Wire transfers must be type of credit"
    //    | _ -> ()

    //    if routingNumber.Length <> 9 then failwith "Invalid routing number length, has to be 9 chars."
    //    let accountHolderName =
    //       if accountHolderName.Length > 22 then accountHolderName.Substring(0,22)
    //       else accountHolderName

    //    let subType =
    //        match transferType with
    //        | CarmelTransferType.ACH secCode ->
    //            Some (secCode.ToString())
    //        | CarmelTransferType.Wire -> None

    //    match address1 with
    //    | None when transferType = CarmelTransferType.Wire -> failwith "Wire transfer: Address 1 has to be defined."
    //    | _ -> ()

    //    let paymentRequest =
    //        JsonData.PaymentOrderRequest.Root(
    //            transferType.ToString(), direction.ToString(),
    //            JsonData.PaymentOrderRequest.ReceivingAccount(
    //                accountNumber,
    //                accountHolderName,
    //                routingNumber,
    //                Some (accountType.ToString()),
    //                address1, None, None),
    //            amountCents (*amount cents*), DateTime.Now.Date (*effective date*),
    //            JsonData.PaymentOrderRequest.StringOrGuid(originationAccount.ToString()), subType, None, None).JsonValue |> Serializer.Serialize

    //    let headers = ["Authorization", $"Bearer {access_token}"; "User-Agent", "F# Client"; "Idempotency-Key", $"p{paymentId}"]

    //    async {
    //        let! res, exn = Utils.makePostRequestWithHeadersAndTimeout ApplicationJson paymentRequest (env.Uri() + "/api/v1/payment-orders") headers
    //        match res, exn with
    //        | r, Some err -> return raise err
    //        | paymentOrder, None ->
    //            let ord = JsonData.PaymentOrderResponse.Load (Serializer.Deserialize paymentOrder)
    //            return ord.PaymentOrder
    //    }

    let createCreditTransfer
        (
            env: CarmelEnvironment,
            access_token: CarmelAccessToken,
            origAccId: Guid,
            paymentId: string,
            amountCents: int,
            transferType: CarmelTransferType,
            direction: CarmelTransferDirection,
            routingNumber: string,
            accountNumber,
            accountHolderName: string,
            accountType: CarmelBankAccountType,
            address1: Option<string>
        ) =

        match direction, transferType with
        | CarmelTransferDirection.Credit, CarmelTransferType.ACH CarmelSecCode.WEB
        | CarmelTransferDirection.Credit, CarmelTransferType.ACH CarmelSecCode.TEL ->
            failwith "Invalid transfer type. ACH Credit has to be PPD or CCD"
        | CarmelTransferDirection.Debit, CarmelTransferType.Wire -> failwith "Wire transfers must be type of credit"
        | _ -> ()

        if routingNumber.Length <> 9 then
            failwith "Invalid routing number length, has to be 9 chars."

        let accountHolderName =
            if accountHolderName.Length > 22 then
                accountHolderName.Substring(0, 22)
            else
                accountHolderName

        let httpClient = makeHttpClient env access_token
        let client = JsonData.CarmelOpenApi.Client httpClient

        async {

            let subscription =
                if logUnsuccessfulHandler.IsSome then
                    Some(JsonData.reportUnsuccessfulEvents logUnsuccessfulHandler.Value)
                else
                    None

            let targetAccount =
                JsonData.CarmelOpenApi.PaymentAccount(
                    accountNumber,
                    accountHolderName,
                    routingNumber
                //, address1, address2, address3
                )

            targetAccount.Type <- accountType.ToString()

            match address1 with
            | None ->
                if transferType <> CarmelTransferType.Wire then
                    ()
                else
                    failwith "Wire transfer: Address 1 has to be defined."
            | Some a -> targetAccount.Address1 <- a

            let createPaymentOrder =
                JsonData.CarmelOpenApi.CreatePaymentOrder(
                    amountCents,
                    DateTime.Now.ToString("yyyy-MM-dd"),
                    origAccId,
                    transferType.ToString(),
                    direction.ToString(),
                    targetAccount,
                    Map.empty //, suplementdata
                //, subtype
                )

            match transferType with
            | CarmelTransferType.ACH secCode -> createPaymentOrder.SubType <- secCode.ToString()
            | CarmelTransferType.Wire -> ()

            let idempotencyKey = $"p{paymentId}"

            let! res = client.PostApiV1PaymentOrders(createPaymentOrder, idempotencyKey) |> Async.Catch
            httpClient.Dispose()

            if subscription.IsSome then
                subscription.Value.Dispose()

            match res with
            | Choice1Of2 x -> return Ok x.PaymentOrder
            | Choice2Of2 err ->
                let details = getErrorDetails err

                //printfn "Used signature: %s" signature_bodyhash_string
                return Error(err, details)
        }

    let fetchCreditTransfer (env: CarmelEnvironment, access_token: CarmelAccessToken, paymentOrderId: Guid) =

        let httpClient = makeHttpClient env access_token
        let client = JsonData.CarmelOpenApi.Client httpClient

        async {

            let subscription =
                if logUnsuccessfulHandler.IsSome then
                    Some(JsonData.reportUnsuccessfulEvents logUnsuccessfulHandler.Value)
                else
                    None

            let! res = client.GetApiV1PaymentOrder paymentOrderId |> Async.Catch
            httpClient.Dispose()

            if subscription.IsSome then
                subscription.Value.Dispose()

            match res with
            | Choice1Of2 x -> return Ok x.PaymentOrder
            | Choice2Of2 err ->
                let details = getErrorDetails err

                //printfn "Used signature: %s" signature_bodyhash_string
                return Error(err, details)
        }

    let fetchCreditTransfers (env: CarmelEnvironment, access_token: CarmelAccessToken, pageId) =

        let httpClient = makeHttpClient env access_token
        let client = JsonData.CarmelOpenApi.Client httpClient

        async {

            let subscription =
                if logUnsuccessfulHandler.IsSome then
                    Some(JsonData.reportUnsuccessfulEvents logUnsuccessfulHandler.Value)
                else
                    None

            let! res = client.GetApiV1PaymentOrders(pageId) |> Async.Catch
            httpClient.Dispose()

            if subscription.IsSome then
                subscription.Value.Dispose()

            match res with
            | Choice1Of2 x -> return Ok x
            | Choice2Of2 err ->
                let details = getErrorDetails err

                //printfn "Used signature: %s" signature_bodyhash_string
                return Error(err, details)
        }

    let approveCreditTransfer
        (
            env: CarmelEnvironment,
            access_token: CarmelAccessToken,
            paymentOrderId: Guid,
            action: CarmelApprovalAction
        ) =

        let httpClient = makeHttpClient env access_token
        let client = JsonData.CarmelOpenApi.Client httpClient

        async {

            let subscription =
                if logUnsuccessfulHandler.IsSome then
                    Some(JsonData.reportUnsuccessfulEvents logUnsuccessfulHandler.Value)
                else
                    None

            let operationArray =
                [| JsonData.CarmelOpenApi.Operation(
                       (match action with
                        | CarmelApprovalAction.Approve -> "approved"
                        | CarmelApprovalAction.Cancel -> "cancelled"),
                       "/status",
                       "replace"
                   ) |]

            let! res = client.PatchApiV1PaymentOrder(paymentOrderId, operationArray) |> Async.Catch
            httpClient.Dispose()

            if subscription.IsSome then
                subscription.Value.Dispose()

            match res with
            | Choice1Of2 x -> return Ok x.PaymentOrder
            | Choice2Of2 err ->
                let details = getErrorDetails err

                //printfn "Used signature: %s" signature_bodyhash_string
                return Error(err, details)
        }


    let fetchEvents (env: CarmelEnvironment, access_token: CarmelAccessToken, page: int) =

        let httpClient = makeHttpClient env access_token
        let client = JsonData.CarmelOpenApi.Client httpClient

        async {

            let subscription =
                if logUnsuccessfulHandler.IsSome then
                    Some(JsonData.reportUnsuccessfulEvents logUnsuccessfulHandler.Value)
                else
                    None

            let! res = client.GetApiV1Events page |> Async.Catch
            httpClient.Dispose()

            if subscription.IsSome then
                subscription.Value.Dispose()

            match res with
            | Choice1Of2 x -> return Ok x
            | Choice2Of2 err ->
                let details = getErrorDetails err

                //printfn "Used signature: %s" signature_bodyhash_string
                return Error(err, details)
        }


    let parseWebhook (respose: string) =
        let r = JsonData.WebhookResponse.Load(Serializer.Deserialize respose)
        r

module CarmelWebhooks =

    open Utils
    open CarmelPayment
    open FSharp.Data
    open FSharp.Data.JsonProvider

    let webhookEvents =
        [| "paymentOrder_approvalRequired"
           "paymentOrder_approved"
           "paymentOrder_cancelled"
           "paymentOrder_corrected"
           "paymentOrder_failed"
           "paymentOrder_remitted"
           "paymentOrder_remitted"
           "paymentOrder_sent" |]

    //let createWebhookSubscription (env:CarmelEnvironment, access_token:CarmelAccessToken, endpointUrl:string, webhookEvents:string[]) =
    //    let subscribeRequest =
    //        JsonData.WebhookSubscriptionRequest.Root(endpointUrl, webhookEvents).JsonValue |> Serializer.Serialize

    //    let authHeader = ["Authorization", $"Bearer {access_token}"; "User-Agent", "F# Client"]

    //    async {
    //        let! res, exn = Utils.makePostRequestWithHeadersAndTimeout ApplicationJson subscribeRequest (env.Uri() + "/api/v1/web-hooks") authHeader
    //        match res, exn with
    //        | r, Some err -> return raise err
    //        | webhookresp, None ->
    //            let wr = JsonData.WebhookSubscriptionResponse.Load (Serializer.Deserialize webhookresp)
    //            return wr
    //    }

    let createWebhookSubscription
        (
            env: CarmelEnvironment,
            access_token: CarmelAccessToken,
            endpointUrl: string,
            webhookEvents: string[]
        ) =

        let httpClient = makeHttpClient env access_token
        let client = JsonData.CarmelOpenApi.Client httpClient

        async {

            let subscription =
                if logUnsuccessfulHandler.IsSome then
                    Some(JsonData.reportUnsuccessfulEvents logUnsuccessfulHandler.Value)
                else
                    None

            let createSubscriber =
                JsonData.CarmelOpenApi.CreateSubscriber(endpointUrl, webhookEvents)

            let! res = client.PostApiV1WebHooks createSubscriber |> Async.Catch
            httpClient.Dispose()

            if subscription.IsSome then
                subscription.Value.Dispose()

            match res with
            | Choice1Of2 x -> return Ok x
            | Choice2Of2 err ->
                let details = getErrorDetails err

                //printfn "Used signature: %s" signature_bodyhash_string
                return Error(err, details)
        }

    //let deleteWebhookSubscription (env:CarmelEnvironment, access_token:CarmelAccessToken, subscriberId:Guid) =

    //    let authHeader = ["Authorization", $"Bearer {access_token}"; "User-Agent", "F# Client"]
    //    let durl = env.Uri() + $"/api/v1/web-hooks/{subscriberId}"

    //    async {
    //        let! res, exn =
    //            Utils.makeVerbRequestWithHeadersAndTimeout HttpVerb.DELETE ApplicationJson durl "" authHeader

    //        match res, exn with
    //        | r, Some err -> return raise err
    //        | webhookresp, None ->
    //            let wr = JsonData.WebhookSubscriptionResponse.Load (Serializer.Deserialize webhookresp)
    //            return wr
    //    }

    let deleteWebhookSubscription (env: CarmelEnvironment, access_token: CarmelAccessToken, subscriberId: Guid) =

        let httpClient = makeHttpClient env access_token
        let client = JsonData.CarmelOpenApi.Client httpClient

        async {

            let subscription =
                if logUnsuccessfulHandler.IsSome then
                    Some(JsonData.reportUnsuccessfulEvents logUnsuccessfulHandler.Value)
                else
                    None

            let! res = client.DeleteApiV1WebHook subscriberId |> Async.Catch
            httpClient.Dispose()

            if subscription.IsSome then
                subscription.Value.Dispose()

            match res with
            | Choice1Of2 x -> return Ok x
            | Choice2Of2 err ->
                let details = getErrorDetails err

                //printfn "Used signature: %s" signature_bodyhash_string
                return Error(err, details)
        }


    //let getWebhookSubscriptions (env:CarmelEnvironment, access_token:CarmelAccessToken) =

    //    let authHeader = ["Authorization", $"Bearer {access_token}"; "User-Agent", "F# Client"]
    //    let gurl = env.Uri() + $"/api/v1/web-hooks"

    //    async {
    //        let! res, exn =
    //            Utils.makeGetRequestWithHeaders ApplicationJson gurl authHeader

    //        match res, exn with
    //        | r, Some err -> return raise err
    //        | webhookresp, None ->
    //            let wr = JsonData.WebhookSubscriptionResponse.Load (Serializer.Deserialize webhookresp)
    //            return wr
    //    }


    let getWebhookSubscriptions (env: CarmelEnvironment, access_token: CarmelAccessToken) =

        let httpClient = makeHttpClient env access_token
        let client = JsonData.CarmelOpenApi.Client httpClient

        async {

            let subscription =
                if logUnsuccessfulHandler.IsSome then
                    Some(JsonData.reportUnsuccessfulEvents logUnsuccessfulHandler.Value)
                else
                    None

            let! res = client.GetApiV1WebHooks() |> Async.Catch
            httpClient.Dispose()

            if subscription.IsSome then
                subscription.Value.Dispose()

            match res with
            | Choice1Of2 x -> return Ok x
            | Choice2Of2 err ->
                let details = getErrorDetails err

                //printfn "Used signature: %s" signature_bodyhash_string
                return Error(err, details)
        }

    let getWebhookSubscription (env: CarmelEnvironment, access_token: CarmelAccessToken, subscriberId: Guid) =

        let httpClient = makeHttpClient env access_token
        let client = JsonData.CarmelOpenApi.Client httpClient

        async {

            let subscription =
                if logUnsuccessfulHandler.IsSome then
                    Some(JsonData.reportUnsuccessfulEvents logUnsuccessfulHandler.Value)
                else
                    None

            let! res = client.GetApiV1WebHook(subscriberId) |> Async.Catch
            httpClient.Dispose()

            if subscription.IsSome then
                subscription.Value.Dispose()

            match res with
            | Choice1Of2 x -> return Ok x
            | Choice2Of2 err ->
                let details = getErrorDetails err

                //printfn "Used signature: %s" signature_bodyhash_string
                return Error(err, details)
        }

    let getWebhookSubscriptionSecret (env: CarmelEnvironment, access_token: CarmelAccessToken, subscriberId: Guid) =

        let authHeader =
            [ "Authorization", $"Bearer {access_token}"; "User-Agent", "F# Client" ]

        let surl = env.Uri() + $"/api/v1/web-hooks/{subscriberId}/secret"

        async {
            let! res, exn = Utils.makeGetRequestWithHeaders ApplicationJson surl authHeader

            match res, exn with
            | r, Some err -> return raise err
            | webhookresp, None ->
                let wr =
                    JsonData.WebhookSubscriptionSecretResponse.Load(Serializer.Deserialize webhookresp)

                return wr.Secret // But where is the secret??
        }

//let getWebhookSubscriptionSecret (env:CarmelEnvironment, access_token:CarmelAccessToken, subscriberId:Guid) =

//    use httpClient = makeHttpClient env access_token
//    let client = JsonData.CarmelOpenApi.Client httpClient
//    async {

//        let subscription =
//            if logUnsuccessfulHandler.IsSome then
//                Some (JsonData.reportUnsuccessfulEvents logUnsuccessfulHandler.Value)
//            else None

//        let! res = client.GetApiV1WebHookSecret subscriberId |> Async.Catch
//        httpClient.Dispose()
//        if subscription.IsSome then
//            subscription.Value.Dispose()
//        match res with
//        | Choice1Of2 x -> return Ok x // schema is wrong.
//        | Choice2Of2 err ->
//            let details = getErrorDetails err

//            //printfn "Used signature: %s" signature_bodyhash_string
//            return Error(err, details)
//    }
