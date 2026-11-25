import { z } from "zod";
import { oc } from "@orpc/contract";
import { webhookAckSchema, webhookBatchSchema, webhookPayloadSchema, webhookQueuedSchema } from "./webhooks.schemas";

export const webhooks = {
  post: oc
    .route({
      method: 'POST',
      path: '/webhooks',
      summary: 'Deliver webhook',
      description: 'Accept a webhook payload',
      operationId: 'deliverWebhook',
      tags: ['Webhooks'],
      successStatus: 202,
      inputStructure: 'detailed',
      spec: (current) => ({ ...current, security: [{ apiKey: [] }] })
    })
    .input(z.object({
      body: z.union([webhookPayloadSchema, webhookBatchSchema])
    }))
    .output(z.union([webhookAckSchema, webhookQueuedSchema, z.string()]))
};
