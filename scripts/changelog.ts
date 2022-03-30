import { exit } from "./lib/utils/utils";
import {
  getPullRequesrs,
  PullRequest,
} from "./lib/changelog/get-pull-requests";
import { renderChangelog } from "./lib/changelog/render-changelog";
import { commitsAfter, getDate, lastTag } from "./lib/utils/git";
import { write } from "./lib/utils/file";
import { genVersion, parseVersion } from "./lib/utils/version";
import { propEq } from "ramda";

const genVersionByPRs = (
  version: string,
  pullRequesrs: PullRequest[]
): string => {
  const isBreaking = Boolean(pullRequesrs.find(propEq("type", "breaking")));
  return genVersion(parseVersion(version), isBreaking).join(".");
};

const genChangeLog = async () => {
  const date = getDate();
  const tag = lastTag();
  const pullRequesrs = await getPullRequesrs(commitsAfter(tag));
  const newVersion = genVersionByPRs(tag, pullRequesrs);
  const changelog = renderChangelog(newVersion, date, pullRequesrs);

  process.env.VERSION = newVersion;
  await write("/dist/changelog.md", changelog);
  process.stdout.write(changelog);
};

genChangeLog().catch(exit);
