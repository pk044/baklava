import { z } from "zod";
import { initContract } from "@ts-rest/core";
import { paymentReceiptSchema, paymentRequestSchema } from "./payments.schemas";

export const payments = initContract().router({
  post: {
    summary: 'Pay for an order',
    description: 'Pay for an order with a card, a bank transfer or cash\n\nRequires authentication: bearerAuth (HTTP Bearer, JWT).',
    method: 'POST',
    path: '/payments',
    body: paymentRequestSchema,
    responses: {
      201: paymentReceiptSchema
    }
  }
});
