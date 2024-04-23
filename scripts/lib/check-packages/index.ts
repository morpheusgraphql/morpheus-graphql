import path from "path";
import { updateDeps } from "./dependencies";
import { getConfig, writeConfig, Config } from "../utils/config";
import { writeYAML } from "../utils/file";
import { genVersion, parseVersion, VersionUpdate } from "../utils/version";
import { log } from "../utils/utils";
import { Bounds } from "../utils/rule";
import { getPackage } from "../utils/package";

const checkPackage = async (
  config: Config,
  isExample: boolean,
  name: string
) => {
  const url = path.join(
    isExample ? path.join("examples", name) : name,
    "package.yaml"
  );
  const pkg = await getPackage(url);

  await writeYAML(url, updateDeps(config, { ...pkg, version: config.version }));

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

  const examples = await Promise.all(
    config.examples.map((name) => checkPackage(config, true, name))
  );

  const libs = await Promise.all(
    config.packages.map((name) => checkPackage(config, false, name))
  );

  await writeConfig(config);

  log(` - package.yaml (v${config.version})\n`, "success");
  log([...libs, ...examples].join(""), "success");
};
