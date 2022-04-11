import { promisify } from "util";
import { readFile, writeFile } from "fs";
import { dirname, join } from "path";
import { dump, load } from "js-yaml";
import { Config, DepsMap } from "../check-packages/types";
import { map } from "ramda";

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
  const { rules, ...rest } = await readYAML<Config>(STACK_CONFIG_URL);

  return {
    ...rest,
    rules: map<DepsMap, DepsMap>(
      ([min, max]) => [min.toString(), max.toString()],
      rules
    ),
  };
};

export const writeConfig = (config: Config) =>
  write(STACK_CONFIG_URL, dump(config, { sortKeys: true }));
