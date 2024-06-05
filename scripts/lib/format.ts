import glob from "glob";
import { exit } from "process";
import { promisify } from "util";
import { exec, log } from "./utils";

const config: Record<string, string> = {
  linux: "/ormolu-Linux.zip",
  win32: "/ormolu-Windows.zip",
  darwin: "/ormolu-macOS.zip",
};

export const platform = process.platform ?? config.linux;

type Ops = { fileName: string; url: string };

export const run = async (
  f: (bin: string) => Promise<void>,
  { fileName, url }: Ops
) => {
  const dir = `__${fileName}__`;
  const bin = `./${dir}/${fileName}`;

  try {
    exec(`mkdir ${dir}`);
  } catch {}

  try {
    log(`setup ${fileName} ... \n`);
    exec(`curl -o ${bin}.zip -LO  ${url}`, "pipe");
    exec(`cd ${dir} && unzip ${fileName}.zip`, "pipe");
    exec(`chmod +x ${bin}`, "pipe");
    await f(bin);
    log("OK\n", "success");
  } catch (e) {
    log(e.message + "\n", "error");
    exec(`rm -rf ${dir}`);
    exit(1);
  }
  exec(`rm -rf ${dir}`);
};

export const format = async ({ fix, path }: { fix: boolean; path: string }) =>
  run(
    async (bin) => {
      const files = await promisify(glob)(path);
      log(`formatting(${files.length} files): ${path} \n\n`);

      if (fix) {
        exec(`${bin} --color=always --mode=inplace ${files.join(" ")}`);
      } else {
        exec(
          `${bin} --color=always --check-idempotence --mode=check ${files.join(
            " "
          )}`
        );
      }
    },
    {
      fileName: "ormolu",
      url: `https://github.com/tweag/ormolu/releases/download/0.5.0.1/${config[platform]}`,
    }
  );
