import { z } from "zod";
import { oc } from "@orpc/contract";
import { categorySchema } from "./categories.schemas";

export const categories = {
  get: oc
    .route({
      method: 'GET',
      path: '/categories',
      summary: 'List categories',
      description: 'The category tree',
      operationId: 'listCategories',
      tags: ['Catalog'],
      successStatus: 200,
      inputStructure: 'detailed'
    })
    .output(z.array(categorySchema))
};
