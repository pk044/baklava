import { BaklavaClient, BaklavaHttpError } from "../client";
import type { WebhookAck, WebhookBatch, WebhookPayload, WebhookQueued } from "./types";

/** Deliver webhook — Accept a webhook payload */
export async function deliverWebhook(client: BaklavaClient, params: {
  body: WebhookPayload | WebhookBatch;
}): Promise<WebhookAck | WebhookQueued | string> {
  const url = new URL(`${client.baseUrl}/webhooks`);
  let __ret!: WebhookAck | WebhookQueued | string;
  const res = await client.fetch(url.toString(), {
    method: "POST",
    headers: {
    "Content-Type": "application/json",
    ...(client.apiKeys?.["X-API-Key"] ? { "X-API-Key": client.apiKeys["X-API-Key"] } : {}),
    },
    body: JSON.stringify(params.body),
  });
  const text = await res.text();
  if (!res.ok) throw new BaklavaHttpError(res.status, text);
  const ct = res.headers.get("content-type") ?? "";
  if (ct.includes("application/json")) {
    return (text ? JSON.parse(text) : undefined) as typeof __ret;
  }
  return text as unknown as typeof __ret;
}
