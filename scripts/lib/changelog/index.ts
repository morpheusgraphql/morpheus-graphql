import { getPullRequests } from "./get-pull-requests";
import { renderChangelog } from "./render-changelog";
import { exec, lastTag } from "../utils/git";
import { propEq } from "ramda";
import { getVersion } from "../utils/config";

export const getChangelog = async () => {
  const version = lastTag();
  const pullRequests = await getPullRequests(version);
  const isBreaking = Boolean(pullRequests.find(propEq("type", "breaking")));

  console.log(exec(`hconf next ${version} ${isBreaking ? "-b" : ""}`));

  const next = await getVersion();

  return {
    body: renderChangelog(next, pullRequests),
    next: next,
  };
};
