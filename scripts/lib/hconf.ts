import { exec } from "./utils";
import { platform, run } from "./format";

const config: Record<string, string> = {
  linux: "hconf-linux.zip",
  win32: "hconf-windows.zip",
  darwin: "hconf-mac-os.zip",
};

const url = `https://github.com/nalchevanidze/hconf/releases/download/0.1.2/${config[platform]}`;

export const setupHconf = async () =>
  run(
    async (bin) => {
      exec(`mkdir cli`);
      exec(`cp ${bin} ./cli/hconf`);
      exec(`echo "$PWD/cli/hconf" >> $GITHUB_PATH`);
    },
    { fileName: "hconf", url }
  );
