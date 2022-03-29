import { promisify } from "util";
import { readFile, writeFile } from "fs";
import path from "path";
import { dump, load } from "js-yaml";

const local = (p: string) => path.join(__dirname, "../../", p);

const read = (url: string) => promisify(readFile)(local(url), "utf8");

const write = (url: string, file: string) =>
  promisify(writeFile)(local(url), file);

export const readYAML = <T>(name: string) =>
  read(name).then(load) as Promise<T>;

export const readJSON = <T>(name: string) =>
  read(name).then((x) => JSON.parse(x)) as Promise<T>;

export const writeYAML = <T>(url: string, obj: T) => write(url, dump(obj));
