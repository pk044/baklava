import { z } from "zod";
import { oc } from "@orpc/contract";
import { paymentReceiptSchema, paymentRequestSchema } from "./payments.schemas";

export const payments = {
  post: oc
    .route({
      method: 'POST',
      path: '/payments',
      summary: 'Pay for an order',
      description: 'Pay for an order with a card, a bank transfer or cash',
      operationId: 'createPayment',
      tags: ['Payments'],
      successStatus: 201,
      inputStructure: 'detailed',
      spec: (current) => ({ ...current, security: [{ bearerAuth: [] }] })
    })
    .input(z.object({
      body: paymentRequestSchema
    }))
    .output(paymentReceiptSchema)
};
