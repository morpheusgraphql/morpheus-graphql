import { promisify } from "util";
import { readFile, writeFile } from "fs";
import { dirname, join } from "path";
import { dump, load } from "js-yaml";

const ROOT_DIR = join(dirname(require.main?.filename ?? ""), "../");

const absolutePath = (p: string) => join(ROOT_DIR, p);

export const read = (url: string) =>
  promisify(readFile)(absolutePath(url), "utf8");

export const write = (url: string, file: string) =>
  promisify(writeFile)(absolutePath(url), file);

export const readYAML = <T>(name: string) =>
  read(name).then(load) as Promise<T>;

export const readJSON = <T>(name: string) =>
  read(name).then((x) => JSON.parse(x)) as Promise<T>;

export const writeYAML = <T>(url: string, obj: T) => write(url, dump(obj));
