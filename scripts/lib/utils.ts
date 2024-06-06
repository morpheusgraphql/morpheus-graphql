import { execSync, StdioOptions } from "child_process";
import { writeFile } from "fs/promises";
import { dirname, join } from "path";
import * as core from "@actions/core";

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

export const exit = (error: Error) => {
  core.setFailed(error);
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

export const hconf = async (
  cmd: "version" | "setup" | "next",
  ...ops: string[]
): Promise<string> => {
  const result = exec(["hconf", [cmd, ops].flat().join(" ")].join(" "));

  if (cmd !== "version") {
    console.log(result);
  }

  return Promise.resolve(result);
};

export const write = (p: string) => (f: string) =>
  writeFile(join(dirname(require.main?.filename ?? ""), "../", p), f, "utf8");

export const setOutput = (name: string) => (value: string) =>
  core.setOutput(name, value);
