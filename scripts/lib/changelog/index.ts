import { getPullRequests, hasBreakingChange } from "./get-pull-requests";
import { renderChangelog } from "./render-changelog";
import { exec, lastTag } from "../git";
import { getVersion } from "../utils";

export const getChangelog = async () => {
  const version = lastTag();
  const prs = await getPullRequests(version);
  const projectVersion = await getVersion();

  console.log(
    exec(`hconf next ${version} ${hasBreakingChange(prs) ? "-b" : ""}`)
  );

  const next = await getVersion();

  return {
    body: renderChangelog(next, prs),
    next: next,
  };
};
