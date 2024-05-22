import { authorizedGithubUrl } from "./gq-api";
import { exec } from "./utils";

const git = (...cmd: string[]) => exec(["git", ...cmd].join(" "));

const getDate = () => git("log", "-1 --format=%cd --date=short");
const lastTag = () => git("describe", "--abbrev=0 --tags");
const commitsAfter = (tag: string) =>
  git("rev-list", "--reverse", `${tag}..`).split("\n");

export const push = (name: string) => {
  git("add", ".");
  git("status");
  git("commit", "-m", `"${name}"`);
  git("push", authorizedGithubUrl(), `HEAD:${name}`);
};

export { getDate, lastTag, commitsAfter, exec };
