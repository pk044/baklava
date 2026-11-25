import { z } from "zod";

export type Category = { "children": Array<Category>; "id": number; "name": string };
export const categorySchema: z.ZodType<Category> = z.object({
        "children": z.array(z.lazy(() => categorySchema)),
        "id": z.number().int(),
        "name": z.string()});
