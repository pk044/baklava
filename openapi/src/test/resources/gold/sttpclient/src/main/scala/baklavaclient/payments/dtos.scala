package baklavaclient.payments

final case class PaymentReceipt(method: io.circe.Json, orderId: Long, reference: io.circe.Json)

final case class PaymentRequest(method: io.circe.Json, note: Option[String] = None, orderId: Long)
