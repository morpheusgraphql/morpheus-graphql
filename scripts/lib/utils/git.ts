import { execSync } from "child_process";

const exec = (command: string) =>
  execSync(command, {
    maxBuffer: 10 * 1024 * 1024, // 10MB
    encoding: "utf-8",
  })?.trimEnd();

const getDate = () => exec("git log -1 --format=%cd --date=short");
const lastTag = () => exec("git describe --abbrev=0 --tags");
const commitsAfter = (tag: string): string[] =>
  exec(`git rev-list --reverse ${tag}..`).split("\n");

export { getDate, lastTag, commitsAfter };
