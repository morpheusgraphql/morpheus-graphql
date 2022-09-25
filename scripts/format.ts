import glob from "glob";
import { stdout } from "process";
import { promisify } from "util";
import { exit, exec } from "./lib/utils/utils";

const ormolu_version = "0.5.0.1";

const config: Record<string, string> = {
  linux:
    "https://github.com/tweag/ormolu/releases/download/" +
    ormolu_version +
    "/ormolu-Linux.zip",
  win32:
    "https://github.com/tweag/ormolu/releases/download/" +
    ormolu_version +
    "/ormolu-Windows.zip",
  darwin:
    "https://github.com/tweag/ormolu/releases/download/" +
    ormolu_version +
    "/ormolu-macOS.zip",
};

const url = config[process.platform] ?? config.linux;

const binary = "./formatter/ormolu";

const main = async () => {
  try {
    exec(`mkdir formatter`);
  } catch {}

  try {
    exec(`curl -o ./formatter/ormolu.zip -LO  ${url}`);
    exec(`cd formatter && unzip ormolu.zip`);

    exec(`chmod +x ${binary}`);

    const files = (await promisify(glob)("./morpheus-graphql*/**/*.hs")).join(
      " "
    );

    //exec(`${binary} --color=always --check-idempotence --mode=check ${files}`);
    exec(`${binary} --color=always --mode=inplace ${files}`);
  } catch (e) {
    stdout.write(e.message);
  }

  exec(`rm -rf ./formatter`);
};

main().catch(exit);
