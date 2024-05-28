import { execSync, spawn, StdioOptions } from "child_process";
import { writeFile } from "fs/promises";
import { dirname, join } from "path";

export const getPRNumber = (msg: string) => {
  const num = / \(#(?<prNumber>[0-9]+)\)$/m.exec(msg)?.groups?.prNumber;
  return num ? parseInt(num, 10) : undefined;
};

export const chunks = <T>(xs: T[]): T[][] => {
  const batches: T[][] = [];

  for (let i = 0; i < xs.length; i += 50) {
    const batch = xs.slice(i, i + 50);
    batches.push(batch);
  }

  return batches;
};

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

export const hconf = (
  cmd: "version" | "setup" | "next",
  ...ops: string[]
): Promise<string> => {
  const p = spawn("hconf", [cmd, ops].flat(), {
    stdio: "pipe",
  });

  return new Promise((res, rej) => {
    p.stdout.on("data", (data) => {
      cmd === "version"
        ? res(data.toString().trim())
        : console.log(data.toString());
    });
    p.on("exit", (code) =>
      code === 0 ? undefined : rej(`Child exited with code ${code}`)
    );
  });
};

export const write = (p: string, f: string) =>
  writeFile(join(dirname(require.main?.filename ?? ""), "../", p), f, "utf8");
