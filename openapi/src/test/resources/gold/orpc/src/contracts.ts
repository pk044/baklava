import { adminConfig } from "./admin/config.contract";
import { adminLoggers } from "./admin/loggers.contract";
import { auth } from "./auth.contract";
import { categories } from "./categories.contract";
import { health } from "./health.contract";
import { me } from "./me.contract";
import { payments } from "./payments.contract";
import { projects } from "./projects.contract";
import { users } from "./users.contract";
import { webhooks } from "./webhooks.contract";

export const contracts = {
  admin: {
    config: adminConfig,
    loggers: adminLoggers
  },
  auth,
  categories,
  health,
  me,
  payments,
  projects,
  users,
  webhooks
};
