import { getPullRequests } from "./get-pull-requests";
import { renderChangelog } from "./render-changelog";
import { commitsAfter, getDate, lastTag } from "../utils/git";
import { formatVersion, genVersion, parseVersion } from "../utils/version";
import { propEq } from "ramda";

export const getChangelog = async () => {
  const date = getDate();
  const version = lastTag();
  const commits = commitsAfter(version);
  const pullRequests = await getPullRequests(commits);
  const isBreaking = Boolean(pullRequests.find(propEq("type", "breaking")));
  const newTag = formatVersion(genVersion(parseVersion(version), isBreaking));

  return {
    body: renderChangelog(newTag, date, pullRequests),
    version: {
      next: newTag,
      prev: version,
      isBreaking,
    },
  };
};
