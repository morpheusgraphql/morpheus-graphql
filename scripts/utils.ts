import { execSync, StdioOptions } from "child_process";

export const exit = (error: Error) => {
  log(error.message, "error");
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
