import { updatePackage } from "./dependencies";
import { Config } from "../utils/config";
import { VersionUpdate } from "../utils/version";
import { log } from "../utils/utils";
import { Package } from "../utils/package";
import { defs } from "../utils/defs";

const checkPackage = (config: Config) => async (dir: string) => {
  const pkg = await Package.read(dir);
  await Package.write(updatePackage(config, pkg), {}, dir);
  return `  - ${pkg.name}\n`;
};

export const checkPackages = async (change?: VersionUpdate) => {
  const config = await Config.load(change);

  const names = await Promise.all(config.packages().map(checkPackage(config)));

  await config.write();

  log(
    [` - ${defs.PACKAGE} (v${config.version})\n`, ...names].join(""),
    "success"
  );
};
