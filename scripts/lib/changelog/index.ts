import { getPullRequests } from "./get-pull-requests";
import { renderChangelog } from "./render-changelog";
import { commitsAfter, exec, getDate, lastTag } from "../utils/git";
import { propEq } from "ramda";
import { getVersion } from "../utils/config";

export const getChangelog = async () => {
  const date = getDate();
  const version = lastTag();
  const pullRequests = await getPullRequests(commitsAfter(version));
  const isBreaking = Boolean(pullRequests.find(propEq("type", "breaking")));

  console.log(exec(`hconf next ${version} ${isBreaking ? "-b" : ""}`));

  const next = await getVersion();

  return {
    body: renderChangelog(next, date, pullRequests),
    next: next,
  };
};
