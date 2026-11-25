import { z } from "zod";

export const bankTransferSchema = z.object({
        "iban": z.string(),
        "reference": z.string()});
export type BankTransfer = z.infer<typeof bankTransferSchema>;

export const cardSchema = z.object({
        "brand": z.string(),
        "last4": z.string()});
export type Card = z.infer<typeof cardSchema>;

export const paymentMethodSchema = z.discriminatedUnion("type", [bankTransferSchema.extend({"type": z.literal("BankTransfer")}), cardSchema.extend({"type": z.literal("Card")}), z.object({"type": z.literal("Cash")})]);
export type PaymentMethod = z.infer<typeof paymentMethodSchema>;

export const paymentReceiptSchema = z.object({
        "method": paymentMethodSchema,
        "orderId": z.number().int(),
        "reference": z.union([z.string(), z.number().int()])});
export type PaymentReceipt = z.infer<typeof paymentReceiptSchema>;

export const paymentRequestSchema = z.object({
        "method": paymentMethodSchema,
        "note": z.string().nullish(),
        "orderId": z.number().int()});
export type PaymentRequest = z.infer<typeof paymentRequestSchema>;
