import { z } from "zod";

export const webhookAckSchema = z.object({
        "received": z.boolean()});
export type WebhookAck = z.infer<typeof webhookAckSchema>;

export const webhookPayloadSchema = z.object({
        "data": z.string(),
        "event": z.string()});
export type WebhookPayload = z.infer<typeof webhookPayloadSchema>;

export const webhookQueuedSchema = z.object({
        "position": z.number().int()});
export type WebhookQueued = z.infer<typeof webhookQueuedSchema>;

export const webhookBatchSchema = z.object({
        "events": z.array(webhookPayloadSchema)});
export type WebhookBatch = z.infer<typeof webhookBatchSchema>;
