import { promisify } from "util";
import { readFile, writeFile } from "fs";
import { dirname, join } from "path";
import { dump, load } from "js-yaml";
import { Config, Rules } from "../check-packages/types";
import { map } from "ramda";
import { compareVersion } from "./version";
import { formatRule, parseRule } from "./rule";

const ROOT_DIR = join(dirname(require.main?.filename ?? ""), "../");

const absolutePath = (p: string) => join(ROOT_DIR, p);
const STACK_CONFIG_URL = "./config/stack.yaml";

export const read = (url: string) =>
  promisify(readFile)(absolutePath(url), "utf8");

export const write = (url: string, file: string) =>
  promisify(writeFile)(absolutePath(url), file);

export const readYAML = <T>(name: string) =>
  read(name).then(load) as Promise<T>;

export const readJSON = <T>(name: string) =>
  read(name).then((x) => JSON.parse(x)) as Promise<T>;

export const writeYAML = <T>(url: string, obj: T) => write(url, dump(obj));

export const getConfig = async (): Promise<Config> => {
  const { rules, ...rest } = await readYAML<Config<string>>(STACK_CONFIG_URL);

  return {
    ...rest,
    rules: map<Rules<string>, Rules>(parseRule, rules),
  };
};

const compareConfigKeys = (a: string, b: string) => {
  try {
    return compareVersion(a, b);
  } catch {
    const x = a.toLowerCase();
    const y = b.toLowerCase();
    if (x < y) {
      return -1;
    }
    if (x > y) {
      return 1;
    }
    return 0;
  }
};

export const writeConfig = (config: Config) => {
  write(
    STACK_CONFIG_URL,
    dump(
      { ...config, rules: map<Rules, Rules<string>>(formatRule, config.rules) },
      {
        sortKeys: compareConfigKeys,
        lineWidth: 240,
        condenseFlow: true,
      }
    )
  );
};
