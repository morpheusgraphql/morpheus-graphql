import path from "path";
import { updateDeps } from "./dependencies";
import { getConfig, writeConfig, Config, updateConfig } from "../utils/config";
import { writeYAML } from "../utils/file";
import { VersionUpdate } from "../utils/version";
import { log } from "../utils/utils";
import { getPackage } from "../utils/package";

const checkPackage = (config: Config) => async (name: string) => {
  const isExample = !name.startsWith("morpheus-graphql");
  const url = path.join(
    isExample ? path.join("examples", name) : name,
    "package.yaml"
  );
  const pkg = await getPackage(url);

  await writeYAML(url, updateDeps(config, { ...pkg, version: config.version }));

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
