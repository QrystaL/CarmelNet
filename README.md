Unofficial .NET client for [Carmel](https://www.carmelsolutions.com/), making bank payments in the US.

Complete API documentation: https://docs.carmelsolutions.com/


To get an access token, call: CarmelPayment.getAcccessToken

To register a new webhook, call: CarmelWebhooks.createWebhookSubscription

To get a list of origination accounts, call: CarmelPayment.getOriginationAccounts

Then to create payments, call: CarmelPayment.createCreditTransfer

And to approve the payments, call: CarmelPayment.approveCreditTransfer

To verify the webhook signatures and parse the response: CarmelWebhooks.parseWebhookResponse

When processing a webhook, fetch the details via: CarmelPayment.fetchCreditTransfer

See the unit tests for examples.

Nuget: https://www.nuget.org/packages/CarmelNet
