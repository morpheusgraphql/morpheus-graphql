import path from "path";
import { Bounds, Config, StackPackage } from "./types";
import { updateDeps } from "./dependencies";
import { getConfig, readYAML, writeConfig, writeYAML } from "../utils/file";
import { genVersion, parseVersion, VersionUpdate } from "../utils/version";
import { log } from "../utils/utils";

const checkPackage = (config: Config) => async (name: string) => {
  const url = path.join(name, "package.yaml");
  const pkg = await readYAML<StackPackage>(url);

  const fixedPackage = updateDeps(config, { ...pkg, version: config.version });

  await writeYAML(url, fixedPackage);

  return `  - ${pkg.name}\n`;
};

const updateConfig = async ({
  next,
  prev,
  isBreaking,
}: VersionUpdate): Promise<Config> => {
  const { version, bounds, ...rest } = await getConfig();

  if (prev !== version) {
    throw new Error(`invalid versions ${version} and ${prev}`);
  }

  const upper = genVersion(parseVersion(next), true).join(".");
  const newBounds: Bounds = isBreaking ? [next, upper] : bounds;

  return {
    ...rest,
    bounds: newBounds,
    version: next,
  };
};

export const checkPackages = async (change?: VersionUpdate) => {
  const config = await (change ? updateConfig(change) : getConfig());

  const versions = await Promise.all(config.packages.map(checkPackage(config)));
  await writeConfig(config);

  log(` - package.yaml (v${config.version})\n`, "success");
  log(versions.join(""), "success");
};
