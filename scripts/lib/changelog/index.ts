import { getPullRequests } from "./get-pull-requests";
import { renderChangelog } from "./render-changelog";
import { commitsAfter, getDate, lastTag } from "../utils/git";
import { Version } from "../utils/version";
import { propEq } from "ramda";

export const getChangelog = async () => {
  const date = getDate();
  const version = lastTag();
  const commits = commitsAfter(version);
  const pullRequests = await getPullRequests(commits);
  const isBreaking = Boolean(pullRequests.find(propEq("type", "breaking")));
  const newTag = new Version(version).next(isBreaking).format();

  return {
    body: renderChangelog(newTag, date, pullRequests),
    version: {
      next: newTag,
      prev: version,
      isBreaking,
    },
  };
};
