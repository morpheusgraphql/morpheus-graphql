import { fetchChanges, isBreaking } from "./fetch";
import { renderChangelog } from "./render-changelog";
import { lastTag } from "../git";
import { hconf } from "../utils";

export const getChangelog = async () => {
  const version = lastTag();
  const projectVersion = await hconf("version");
  const changes = await fetchChanges(version);

  if (version !== projectVersion) {
    throw Error(`versions does not match: ${version} ${projectVersion}`);
  }

  console.log(hconf("next", ...(isBreaking(changes) ? ["-b"] : [])));

  const next = await hconf("version");

  return {
    body: renderChangelog(next, changes),
    next,
  };
};
