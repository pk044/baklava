import { z } from "zod";
import { initContract } from "@ts-rest/core";
import { webhookAckSchema, webhookBatchSchema, webhookPayloadSchema, webhookQueuedSchema } from "./webhooks.schemas";

export const webhooks = initContract().router({
  post: {
    summary: 'Deliver webhook',
    description: 'Accept a webhook payload\n\nRequires authentication: apiKey (API key in header X-API-Key).',
    method: 'POST',
    path: '/webhooks',
    body: z.union([webhookPayloadSchema, webhookBatchSchema]),
    responses: {
      202: z.union([webhookAckSchema, webhookQueuedSchema, z.string()])
    }
  }
});
