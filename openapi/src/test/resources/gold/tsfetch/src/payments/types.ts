export interface BankTransfer {
  iban: string;
  reference: string;
}

export interface Card {
  brand: string;
  last4: string;
}

export interface PaymentReceipt {
  method: ({ type: "BankTransfer" } & BankTransfer) | ({ type: "Card" } & Card) | ({ type: "Cash" } & Record<string, unknown>);
  orderId: number;
  reference: string | number;
}

export interface PaymentRequest {
  method: ({ type: "BankTransfer" } & BankTransfer) | ({ type: "Card" } & Card) | ({ type: "Cash" } & Record<string, unknown>);
  note?: string | null;
  orderId: number;
}
