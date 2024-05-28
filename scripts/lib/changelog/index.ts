import { fetchChanges, isBreaking } from "./fetch";
import { render } from "./render";
import { lastTag } from "../git";
import { exec, hconf } from "../utils";

export const changelog = async (change: boolean = false) => {
  const version = lastTag();
  const projectVersion = await hconf("version");
  const changes = await fetchChanges(version);

  if (version !== projectVersion) {
    throw Error(`versions does not match: ${version} ${projectVersion}`);
  }

  await hconf("next", ...(isBreaking(changes) ? ["-b"] : []));

  if (change) {
    await hconf("setup");

    console.log(exec("stack build --test --dry-run"));
    console.log(exec("stack sdist"));
  }

  return render(await hconf("version"), changes);
};
