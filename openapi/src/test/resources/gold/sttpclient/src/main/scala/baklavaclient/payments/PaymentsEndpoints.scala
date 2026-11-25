package baklavaclient.payments

import sttp.client4._
import sttp.client4.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import sttp.model.Uri

object PaymentsEndpoints {

  /** Pay for an order — Pay for an order with a card, a bank transfer or cash */
  def createPayment(
      baseUri: Uri,
      bearerAuthToken: String
  )(
      body: PaymentRequest
  ): Request[Either[ResponseException[String], PaymentReceipt]] = {
    basicRequest
      .post(baseUri.addPath("payments"))
      .header("Authorization", s"Bearer ${bearerAuthToken}")
      .body(body.asJson.noSpaces)
      .contentType("application/json")
      .response(asJson[PaymentReceipt])
  }
}
