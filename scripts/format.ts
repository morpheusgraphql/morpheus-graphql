import glob from "glob";
import { exit, stdout } from "process";
import { promisify } from "util";
import { exec } from "./lib/utils/utils";
import { Command } from "commander";

const cli = new Command();

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

const format = async ({ fix }: { fix: boolean }) => {
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

    if (fix) {
      exec(`${binary} --color=always --no-cabal --mode=inplace ${files}`);
    } else {
      exec(
        `${binary} --color=always --no-cabal --check-idempotence --mode=check ${files}`
      );
    }
  } catch (e) {
    stdout.write(e.message);
    exit(1);
  }

  exec(`rm -rf ./formatter`);
};

cli.name("format").description("format").version("0.0.0");
cli.option("--fix <boolean>", "fix", false).action(format);

cli.parse();
