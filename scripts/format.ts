import glob from "glob";
import { exit, stdout } from "process";
import { promisify } from "util";
import { exec } from "./lib/utils/utils";
import { Command } from "commander";

const cli = new Command();

const config: Record<string, string> = {
  linux: "/ormolu-Linux.zip",
  win32: "/ormolu-Windows.zip",
  darwin: "/ormolu-macOS.zip",
};

const name = config[process.platform] ?? config.linux;

const binary = "./formatter/ormolu";

const format = async ({ fix }: { fix: boolean }) => {
  try {
    exec(`mkdir formatter`);
  } catch {}

  try {
    exec(
      `curl -o ./formatter/ormolu.zip -LO  https://github.com/tweag/ormolu/releases/download/0.5.0.1/${name}`
    );
    exec(`cd formatter && unzip ormolu.zip`);

    exec(`chmod +x ${binary}`);

    const files = (await promisify(glob)("./morpheus-graphql*/**/*.hs")).join(
      " "
    );

    if (fix) {
      exec(`${binary} --color=always --mode=inplace ${files}`);
    } else {
      exec(
        `${binary} --color=always --check-idempotence --mode=check ${files}`
      );
      stdout.write("OK");
    }
  } catch (e) {
    stdout.write(e.message);
    exec(`rm -rf ./formatter`);
    exit(1);
  }

  exec(`rm -rf ./formatter`);
};

cli.name("format").description("format").version("0.0.0");
cli.option("--fix <boolean>", "fix", false).action(format);

cli.parse();
