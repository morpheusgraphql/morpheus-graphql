import path from "path";
import { Config, StackPackage } from "./lib/types";
import { updateDeps } from "./lib/dependencies";
import { readJSON, readYAML, writeYAML } from "./lib/file";

export const checkPackage = (config: Config) => async (name: string) => {
  const url = path.join(name, "package.yaml");
  const pkg = await readYAML<StackPackage>(url);

  const fixedPackage = updateDeps(config, { ...pkg, version: config.version });

  await writeYAML(url, fixedPackage);

  console.info(`Checked:\n  ${pkg.name}: ${pkg.version}`);
};

const main = async () => {
  const config = await readJSON<Config>("config.json");

  await Promise.all(config.packages.map(checkPackage(config)));
};

main().catch((err) => {
  console.error(err.message);
  process.exit(1);
});
