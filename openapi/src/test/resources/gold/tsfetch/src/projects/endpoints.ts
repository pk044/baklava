import { BaklavaClient, BaklavaHttpError } from "../client";
import type { CreateProjectRequest, CreateTaskRequest, PatchProjectRequest, Project, Task } from "./types";
import type { ErrorResponse } from "../common/types";

/** List projects — List projects, optionally filtered by status */
export async function listProjects(client: BaklavaClient, params?: {
  status?: "active" | "archived" | "draft";
}): Promise<Project[]> {
  const url = new URL(`${client.baseUrl}/projects`);
  if (params?.status !== undefined) url.searchParams.set("status", String(params.status));
  let __ret!: Project[];
  const res = await client.fetch(url.toString(), {
    method: "GET",
    headers: {
    ...client.authHeaders(),
    },
  });
  const text = await res.text();
  if (!res.ok) throw new BaklavaHttpError(res.status, text);
  const ct = res.headers.get("content-type") ?? "";
  if (ct.includes("application/json")) {
    return (text ? JSON.parse(text) : undefined) as typeof __ret;
  }
  return text as unknown as typeof __ret;
}

/** Create project — Create a new project */
export async function createProject(client: BaklavaClient, params: {
  body: CreateProjectRequest;
}): Promise<Project> {
  const url = new URL(`${client.baseUrl}/projects`);
  let __ret!: Project;
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

/** Patch project — Partially update a project */
export async function patchProject(client: BaklavaClient, params: {
  projectId: number;
  body: PatchProjectRequest;
}): Promise<Project> {
  const url = new URL(`${client.baseUrl}/projects/${encodeURIComponent(String(params.projectId))}`);
  let __ret!: Project;
  const res = await client.fetch(url.toString(), {
    method: "PATCH",
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

/** List tasks — List all tasks in a project */
export async function listTasks(client: BaklavaClient, params: {
  projectId: number;
  label: string[];
  assigneeIds: number[];
  fields?: string[];
}): Promise<Task[]> {
  const url = new URL(`${client.baseUrl}/projects/${encodeURIComponent(String(params.projectId))}/tasks`);
  if (params?.label !== undefined) for (const value of params.label) url.searchParams.append("label", String(value));
  if (params?.assigneeIds !== undefined && params.assigneeIds.length > 0) url.searchParams.set("assigneeIds", params.assigneeIds.map(String).join(","));
  if (params?.fields !== undefined && params.fields.length > 0) url.searchParams.set("fields", params.fields.map(String).join("|"));
  let __ret!: Task[];
  const res = await client.fetch(url.toString(), {
    method: "GET",
    headers: {
    ...client.authHeaders(),
    },
  });
  const text = await res.text();
  if (!res.ok) throw new BaklavaHttpError(res.status, text);
  const ct = res.headers.get("content-type") ?? "";
  if (ct.includes("application/json")) {
    return (text ? JSON.parse(text) : undefined) as typeof __ret;
  }
  return text as unknown as typeof __ret;
}

/** Create task — Create a task in a project */
export async function createTask(client: BaklavaClient, params: {
  projectId: number;
  body: CreateTaskRequest;
}): Promise<Task> {
  const url = new URL(`${client.baseUrl}/projects/${encodeURIComponent(String(params.projectId))}/tasks`);
  let __ret!: Task;
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
