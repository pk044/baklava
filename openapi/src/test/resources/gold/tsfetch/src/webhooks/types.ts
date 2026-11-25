export interface WebhookAck {
  received: boolean;
}

export interface WebhookBatch {
  events: WebhookPayload[];
}

export interface WebhookPayload {
  data: string;
  event: string;
}

export interface WebhookQueued {
  position: number;
}
