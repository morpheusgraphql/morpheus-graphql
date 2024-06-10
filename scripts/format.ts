import glob from "glob";
import { promisify } from "util";
import { exec, log, exit } from "./utils";

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
    exec(`rm -rf ${dir}`);
    exit(e);
  }
  exec(`rm -rf ${dir}`);
};

export const format = async ({ fix, path }: { fix: boolean; path: string }) =>
  run(
    async (bin) => {
      const fs = await promisify(glob)(path);
      log(`formatting(${fs.length} files): ${path} \n\n`);
      const files = fs.join(" ");

      if (fix) {
        exec(`${bin} --color=always --mode=inplace ${files}`);
      } else {
        exec(`${bin} --color=always --check-idempotence --mode=check ${files}`);
      }
    },
    {
      fileName: "ormolu",
      url: `https://github.com/tweag/ormolu/releases/download/0.5.0.1/${config[platform]}`,
    }
  ).catch(exit);
