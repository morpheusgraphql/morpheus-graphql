import { exec } from "./utils";

const git = (...cmd: string[]) => exec(["git", ...cmd].join(" "));

const getDate = () => git("log", "-1", "--format=%cd", "--date=short");
const lastTag = () => git("describe", "--abbrev=0", "--tags");
const commitsAfter = (tag: string) =>
  git("rev-list", "--reverse", `${tag}..`).split("\n");

export { git, getDate, lastTag, commitsAfter, exec };
