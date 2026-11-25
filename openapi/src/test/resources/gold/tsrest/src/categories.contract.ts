import { z } from "zod";
import { initContract } from "@ts-rest/core";
import { categorySchema } from "./categories.schemas";

export const categories = initContract().router({
  get: {
    summary: 'List categories',
    description: 'The category tree',
    method: 'GET',
    path: '/categories',
    responses: {
      200: z.array(categorySchema)
    }
  }
});
