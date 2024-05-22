import { authorizedGithubUrl } from "./gq-api";
import { exec } from "./utils";

const getDate = () => exec("git log -1 --format=%cd --date=short");
const lastTag = () => exec("git describe --abbrev=0 --tags");
const commitsAfter = (tag: string): string[] =>
  exec(`git rev-list --reverse ${tag}..`).split("\n");

export const push = (name: string) => {
  exec("git add .");
  exec("git status");
  exec(`git commit -m "${name}"`);
  exec(`git push ${authorizedGithubUrl()} HEAD:${name}`);
};

export { getDate, lastTag, commitsAfter, exec };
