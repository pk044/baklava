package baklavaclient.webhooks

final case class WebhookAck(received: Boolean)

final case class WebhookBatch(events: Seq[WebhookPayload])

final case class WebhookPayload(data: String, event: String)

final case class WebhookQueued(position: Int)
