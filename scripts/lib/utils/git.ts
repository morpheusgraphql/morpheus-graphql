import { GH_ORG, GH_REPO, GITHUB_TOKEN } from "./gq-api";
import { exec } from "./utils";

const getDate = () => exec("git log -1 --format=%cd --date=short");
const lastTag = () => exec("git describe --abbrev=0 --tags");
const commitsAfter = (tag: string): string[] =>
  exec(`git rev-list --reverse ${tag}..`).split("\n");

export const push = (name: string) => {
  exec("git add .");
  exec("git status");
  exec(`git commit -m "${name}"`);
  exec(
    `git push https://${GITHUB_TOKEN}@github.com/${GH_ORG}/${GH_REPO}.git HEAD:${name}`
  );
};

export { getDate, lastTag, commitsAfter, exec };
