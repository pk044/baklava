package baklavaclient.webhooks

import sttp.client4._
import sttp.model.Uri

object WebhooksEndpoints {

  /** Deliver webhook — Accept a webhook payload */
  def deliverWebhook(
      baseUri: Uri,
      apiKeyValue: String
  )(
      bodyJson: String
  ): Request[Either[String, String]] = {
    basicRequest
      .post(baseUri.addPath("webhooks"))
      .header("X-API-Key", apiKeyValue)
      .body(bodyJson)
      .contentType("application/json")
  }
}
