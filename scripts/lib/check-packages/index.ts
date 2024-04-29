import { updatePackage } from "./dependencies";
import { Config } from "../utils/config";
import { VersionUpdate } from "../utils/version";
import { log } from "../utils/utils";
import { Package } from "../utils/package";

const checkPackage = (config: Config) => async (dir: string) => {
  const pkg = await Package.read(dir);
  await Package.write(dir, updatePackage(config, pkg));
  return `  - ${pkg.name}\n`;
};

export const checkPackages = async (change?: VersionUpdate) => {
  const config = await Config.read(change);

  const names = await Promise.all(config.packages().map(checkPackage(config)));

  await config.write();

  log([` - package.yaml (v${config.version})\n`, ...names].join(""), "success");
};
