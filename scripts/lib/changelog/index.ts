import { fetchChanges, isBreaking } from "./fetch";
import { render } from "./render";
import { lastTag } from "../git";
import { hconf } from "../utils";
import { Config } from "./types";

export const changelog = async (config: Config, change: boolean = false) => {
  const version = lastTag();
  const projectVersion = await hconf("version");
  const changes = await fetchChanges(config, version);

  if (version !== projectVersion) {
    throw Error(`versions does not match: ${version} ${projectVersion}`);
  }

  await hconf("next", ...(isBreaking(changes) ? ["-b"] : []));

  if (change) {
    await hconf("setup");
  }

  return render(config, await hconf("version"), changes);
};
