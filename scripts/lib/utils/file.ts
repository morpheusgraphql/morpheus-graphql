import { promisify } from "util";
import { readFile, writeFile } from "fs";
import { dirname, join } from "path";
import { dump, load } from "js-yaml";
import { Config } from "../check-packages/types";

const ROOT_DIR = join(dirname(require.main?.filename ?? ""), "../");

const absolutePath = (p: string) => join(ROOT_DIR, p);
const STACK_CONFIG_URL = "./config/stack.json";

export const read = (url: string) =>
  promisify(readFile)(absolutePath(url), "utf8");

export const write = (url: string, file: string) =>
  promisify(writeFile)(absolutePath(url), file);

export const readYAML = <T>(name: string) =>
  read(name).then(load) as Promise<T>;

export const readJSON = <T>(name: string) =>
  read(name).then((x) => JSON.parse(x)) as Promise<T>;

export const writeYAML = <T>(url: string, obj: T) => write(url, dump(obj));

export const getConfig = () => readJSON<Config>(STACK_CONFIG_URL);

export const writeConfig = (config: Config) =>
  write(STACK_CONFIG_URL, JSON.stringify(config, null, 2));
