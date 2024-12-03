namespace CarmelUnitTests

open System
open CarmelNet
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type Test1() =

    // You will get clientId and Secret from Carmel
    let clientId = ""
    let clientSecret = ""
    // This is just id from URL of https://webhook.site/
    let webhookSiteGuid = ""

    let webhookTestEndpoint = "https://webhook.site/#!/view/" + webhookSiteGuid
    let rnd = System.Random()

    [<TestMethod>]
    member this.AuthTest() =
        async {
            let! access_token = CarmelPayment.getAcccessToken (CarmelEnvironment.Sandbox, clientId, clientSecret)

            Assert.IsTrue(access_token.ToString() <> """{"error":"invalid_request"}""")

        }
        |> Async.RunSynchronously


    [<TestMethod>]
    member this.GetOriginationAccountTest() =
        async {
            let! access_token = CarmelPayment.getAcccessToken (CarmelEnvironment.Sandbox, clientId, clientSecret)
            let! origAcc = CarmelPayment.getOriginationAccounts (CarmelEnvironment.Sandbox, access_token)

            //Assert.IsTrue(origAcc.OriginationAccounts.Length > 0);

            let accs =
                match origAcc with
                | Ok acc -> acc
                | Error(err, x) -> failwith (err.ToString())

            Assert.IsTrue(accs.Length > 0)

        }
        |> Async.RunSynchronously

    [<TestMethod>]
    member this.GetWebhooksTest() =
        async {

            let! access_token = CarmelPayment.getAcccessToken (CarmelEnvironment.Sandbox, clientId, clientSecret)

            let! subscribed = CarmelWebhooks.getWebhookSubscriptions (CarmelEnvironment.Sandbox, access_token)

            match subscribed with
            | Ok x ->
                Assert.IsNotNull x

                if isNull x.Subscriber then
                    // Note: For some reason this seems to return null. Use rather CarmelWebhooks.getWebhookSubscription with subscriberId parameter.
                    printfn "No subscriptions found"
                else
                    printfn "Subscription found: %s" (x.Subscriber.ToString())

            | Error(err, txt) ->
                printfn "Error getting webhooks: %s" txt
                raise err

        }
        |> Async.RunSynchronously

    [<TestMethod>]
    member this.RegisterAndRemoveWebHook() =
        async {
            let webhooks = CarmelWebhooks.webhookEvents

            let! access_token = CarmelPayment.getAcccessToken (CarmelEnvironment.Sandbox, clientId, clientSecret)

            let! subscribed =
                CarmelWebhooks.createWebhookSubscription (CarmelEnvironment.Sandbox, access_token, webhookTestEndpoint, webhooks)

            let subscriberId =
                match subscribed with
                | Ok x ->
                    Assert.IsTrue(x.Subscriber.Id.IsSome, "Webhook subscription not found")
                    Assert.AreEqual<String>("active", x.Subscriber.Status)
                    x.Subscriber.Id.Value
                | Error(err, txt) ->
                    printfn "Error subscribing webhook: %s" txt
                    raise err

            let! webhookSecret =
                CarmelWebhooks.getWebhookSubscriptionSecret (CarmelEnvironment.Sandbox, access_token, subscriberId)

            Assert.IsFalse(String.IsNullOrEmpty webhookSecret, "Webhook secret not found")

            printfn "Webhook %O wired to: %s with secret %s" subscriberId webhookTestEndpoint webhookSecret

            let! deleted =
                CarmelWebhooks.deleteWebhookSubscription (CarmelEnvironment.Sandbox, access_token, subscriberId)

            let delResp =
                match deleted with
                | Ok x ->
                    Assert.IsFalse(String.IsNullOrEmpty(x.ToString()), "Webhook subscription not found")
                    x
                | Error(err, txt) ->
                    printfn "Error deleting webhook: %s" txt
                    raise err

            return ()
        }
        |> Async.RunSynchronously

    [<TestMethod>]
    member this.CreateCreditTransferTest() =
        async {
            let paymentId = rnd.NextInt64(100_000, 999_999).ToString()

            let! access_token = CarmelPayment.getAcccessToken (CarmelEnvironment.Sandbox, clientId, clientSecret)
            let! origAccs = CarmelPayment.getOriginationAccounts (CarmelEnvironment.Sandbox, access_token)

            let paymentAccIc =
                match origAccs with
                | Ok acc ->
                    let firstAcc = (acc |> Seq.head)
                    firstAcc.Id.Value
                | Error(err, txt) ->
                    printfn "Error fetching orig account: %s" txt
                    raise err

            let! creditTransfer =
                CarmelPayment.createCreditTransfer (
                    CarmelEnvironment.Sandbox,
                    access_token,
                    paymentAccIc,
                    paymentId,
                    100,
                    CarmelTransferType.ACH CarmelSecCode.PPD,
                    CarmelTransferDirection.Debit,
                    "091809524",
                    "1234567",
                    "Tuomas Testing",
                    CarmelBankAccountType.Checking,
                    None
                )

            match creditTransfer with
            | Error(err, txt) ->
                printfn "Error making credit transfer: %s" txt
                raise err
            | Ok resp ->
                Assert.IsTrue(resp.Id.IsSome, "Response Id not found")
                // Debit, ACH WEB: "approvalRequired"
                // Credit, ACH PPD: "approvalRequired"
                // Debit, ACH PPD: "approvalRequired"

                Assert.AreEqual<String>("approvalRequired", resp.Status)
                let paymentOrderId = resp.Id.Value

                let! approval =
                    CarmelPayment.approveCreditTransfer (
                        CarmelEnvironment.Sandbox,
                        access_token,
                        paymentOrderId,
                        CarmelApprovalAction.Approve
                    )

                match approval with
                | Error(err, txt) ->
                    printfn "Error approving credit transfer: %s" txt
                    raise err
                | Ok res2 ->
                    Assert.IsTrue(res2.Id.IsSome, "Response Id not found")
                    Assert.AreEqual<String>("approved", resp.Status)

                ()

        }
        |> Async.RunSynchronously


    [<TestMethod>]
    member this.FetchCreditTransfersTest() =
        async {

            let! access_token = CarmelPayment.getAcccessToken (CarmelEnvironment.Sandbox, clientId, clientSecret)

            let pageId = 1

            let! creditTransfer =
                CarmelPayment.fetchCreditTransfers (CarmelEnvironment.Sandbox, access_token, pageId, None, None, None)

            match creditTransfer with
            | Error(err, txt) ->
                printfn "Error fetching credit transfers: %s" txt
                raise err
            | Ok resp ->
                Assert.IsNotNull(resp, "Response not found")

                ()

        }
        |> Async.RunSynchronously

    [<TestMethod>]
    member this.FetchEventsTest() =
        async {

            let! access_token = CarmelPayment.getAcccessToken (CarmelEnvironment.Sandbox, clientId, clientSecret)

            let pageId = 1

            let! creditTransfer =
                CarmelPayment.fetchEvents (CarmelEnvironment.Sandbox, access_token, pageId, None, None, None)

            match creditTransfer with
            | Error(err, txt) ->
                printfn "Error fetching events: %s" txt
                raise err
            | Ok resp ->
                Assert.IsNotNull(resp, "Response not found")

                ()

        }
        |> Async.RunSynchronously
