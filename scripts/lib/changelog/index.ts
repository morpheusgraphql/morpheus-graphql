import { getPullRequesrs } from "./get-pull-requests";
import { renderChangelog } from "./render-changelog";
import { commitsAfter, getDate, lastTag } from "../utils/git";
import { genVersion, parseVersion } from "../utils/version";
import { propEq } from "ramda";

export const getChangelog = async () => {
  const date = getDate();
  const version = lastTag();
  const commits = commitsAfter(version);
  const pullRequesrs = await getPullRequesrs(commits);
  const isBreaking = Boolean(pullRequesrs.find(propEq("type", "breaking")));
  const nextVersion = genVersion(parseVersion(version), isBreaking);
  const newTag = nextVersion.join(".");

  return {
    body: renderChangelog(newTag, date, pullRequesrs),
    version: {
      next: newTag,
      prev: version,
      isBreaking,
    },
  };
};
