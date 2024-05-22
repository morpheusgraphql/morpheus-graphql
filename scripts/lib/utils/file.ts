import { readFile, writeFile } from "fs/promises";
import { dirname, join } from "path";
import { load } from "js-yaml";

const ROOT = join(dirname(require.main?.filename ?? ""), "../");

const absolute = (p: string) => join(ROOT, p);

export const write = (url: string, file: string) =>
  writeFile(absolute(url), file, "utf8");

export const getVersion = () =>
  readFile(absolute("./hconf.yaml"), "utf8")
    .then((s) => load(s) as { version: string })
    .then(({ version }) => version);
