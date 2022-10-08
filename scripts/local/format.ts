import glob from "glob";
import { exit } from "process";
import { promisify } from "util";
import { exec, log } from "../lib/utils/utils";

const config: Record<string, string> = {
  linux: "/ormolu-Linux.zip",
  win32: "/ormolu-Windows.zip",
  darwin: "/ormolu-macOS.zip",
};

const name = config[process.platform] ?? config.linux;

const binary = "./formatter/ormolu";

export const format = async ({ fix, path }: { fix: boolean; path: string }) => {
  try {
    exec(`mkdir formatter`);
  } catch {}

  try {
    log("setup ormolu\n");

    exec(
      `curl -o ./formatter/ormolu.zip -LO  https://github.com/tweag/ormolu/releases/download/0.5.0.1/${name}`,
      "pipe"
    );
    exec(`cd formatter && unzip ormolu.zip`, "pipe");

    exec(`chmod +x ${binary}`, "pipe");

    const files = await promisify(glob)(path);

    log(`start formatting: ${path} (${files.length} files)\n\n`);

    if (fix) {
      exec(`${binary} --color=always --mode=inplace ${files.join(" ")}`);
    } else {
      exec(
        `${binary} --color=always --check-idempotence --mode=check ${files.join(
          " "
        )}`
      );

      log("OK\n", "success");
    }
  } catch (e) {
    log(e.message + "\n", "error");
    exec(`rm -rf ./formatter`);
    exit(1);
  }

  exec(`rm -rf ./formatter`);
};
