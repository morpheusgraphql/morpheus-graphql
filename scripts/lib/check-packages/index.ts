import path from "path";
import { updateDeps } from "./dependencies";
import { getConfig, writeConfig, Config, updateConfig } from "../utils/config";
import { VersionUpdate } from "../utils/version";
import { log } from "../utils/utils";
import { getPackage, writePackage } from "../utils/package";

const checkPackage = (config: Config) => async (name: string) => {
  const dir = name.startsWith("morpheus-graphql")
    ? name
    : path.join("examples", name);

  const pkg = await getPackage(dir);

  await writePackage(
    dir,
    updateDeps(config, { ...pkg, version: config.version })
  );

  return `  - ${pkg.name}\n`;
};

export const checkPackages = async (change?: VersionUpdate) => {
  const config = await (change ? updateConfig(change) : getConfig());
  const { version, examples, packages } = config;

  const libs = await Promise.all(
    [...packages, ...examples].map(checkPackage(config))
  );

  await writeConfig(config);

  log([` - package.yaml (v${version})\n`, ...libs].join(""), "success");
};
