import { getPullRequests, isBreaking } from "./get-pull-requests";
import { renderChangelog } from "./render-changelog";
import { exec, lastTag } from "../git";
import { getVersion } from "../utils";

export const getChangelog = async () => {
  const version = lastTag();
  const prs = await getPullRequests(version);
  const projectVersion = await getVersion();

  if (version !== projectVersion) {
    throw Error(`versions does not match: ${version} ${projectVersion}`);
  }

  console.log(exec(`hconf next ${isBreaking(prs) ? "-b" : ""}`));

  const next = await getVersion();

  return {
    body: renderChangelog(next, prs),
    next: next,
  };
};
