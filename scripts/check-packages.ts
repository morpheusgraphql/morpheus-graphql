import path from "path";
import { Config, StackPackage } from "./lib/check-packages/types";
import { updateDeps } from "./lib/check-packages/dependencies";
import { readJSON, readYAML, writeYAML } from "./lib/utils/file";
import { exit } from "./lib/utils/utils";

export const checkPackage = (config: Config) => async (name: string) => {
  const url = path.join(name, "package.yaml");
  const pkg = await readYAML<StackPackage>(url);

  const fixedPackage = updateDeps(config, { ...pkg, version: config.version });

  await writeYAML(url, fixedPackage);

  console.info(`Checked:\n  ${pkg.name}: ${pkg.version}`);
};

const checkPackages = async () => {
  const config = await readJSON<Config>("config.json");

  await Promise.all(config.packages.map(checkPackage(config)));
};

checkPackages().catch(exit);
