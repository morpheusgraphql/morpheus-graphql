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
      try {
        exec(`mkdir $HOME/.local/bin`);
      } catch {}

      exec(`cp ${bin} $HOME/.local/bin/hconf`);
      exec("hconf version");
    },
    { fileName: "hconf", url }
  );
