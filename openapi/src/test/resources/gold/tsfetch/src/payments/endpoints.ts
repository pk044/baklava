import { BaklavaClient, BaklavaHttpError } from "../client";
import type { BankTransfer, Card, PaymentReceipt, PaymentRequest } from "./types";

/** Pay for an order — Pay for an order with a card, a bank transfer or cash */
export async function createPayment(client: BaklavaClient, params: {
  body: PaymentRequest;
}): Promise<PaymentReceipt> {
  const url = new URL(`${client.baseUrl}/payments`);
  let __ret!: PaymentReceipt;
  const res = await client.fetch(url.toString(), {
    method: "POST",
    headers: {
    ...client.authHeaders(),
    "Content-Type": "application/json",
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
