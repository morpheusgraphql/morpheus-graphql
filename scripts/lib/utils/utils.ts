import { execSync, StdioOptions } from "child_process";

export const getPRNumber = (msg: string) => {
  const num = / \(#(?<prNumber>[0-9]+)\)$/m.exec(msg)?.groups?.prNumber;
  return num ? parseInt(num, 10) : undefined;
};

const chunks = <T>(commits: T[]): T[][] => {
  const batches: T[][] = [];

  for (let i = 0; i < commits.length; i += 50) {
    const batch = commits.slice(i, i + 50);
    batches.push(batch);
  }

  return batches;
};

export const batchMap = <I, O>(f: (_: I[]) => Promise<O[]>, commits: I[]) =>
  Promise.all(chunks(commits).map((batch) => f(batch))).then((x) => x.flat());

export const isKey = <T extends string>(
  obj: Record<T, unknown>,
  key?: string | null
): key is T => typeof key === "string" && key in obj;

export const exit = (error: unknown) => {
  console.error(error);
  process.exit(1);
};

export const exec = (command: string, stdio?: StdioOptions) =>
  execSync(command, {
    maxBuffer: 10 * 1024 * 1024, // 10MB
    encoding: "utf-8",
    stdio,
  })?.trimEnd();

const colors = {
  error: "\x1b[31m",
  success: "\x1b[32m",
  warning: "\x1b[33m",
  none: "\x1b[0m",
};

export const log = (t: string, type?: "success" | "warning" | "error") =>
  process.stdout.write(colors[type ?? "none"] + t + colors.none);
