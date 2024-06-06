import { fetchChanges, isBreaking } from "./fetch";
import { render } from "./render";
import { lastTag } from "../git";
import { hconf } from "../utils";
import { Config } from "./types";

const config: Config = {
  scope: {
    server: "morpheus-graphql",
    client: "morpheus-graphql-client",
    core: "morpheus-graphql-core",
    subscriptions: "morpheus-graphql-subscriptions",
    tests: "morpheus-graphql-tests",
    app: "morpheus-graphql-app",
  },
  pr: {
    breaking: "Breaking Change",
    feature: "New features",
    fix: "Bug Fixes",
    chore: "Minor Changes",
  },
};

export const changelog = async (change: boolean = false) => {
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
