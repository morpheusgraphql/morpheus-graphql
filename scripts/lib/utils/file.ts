import { readFile, writeFile } from "fs/promises";
import { dirname, join } from "path";
import { DumpOptions, dump, load } from "js-yaml";

const ROOT = join(dirname(require.main?.filename ?? ""), "../");

const absolute = (p: string) => join(ROOT, p);

export const read = (url: string) => readFile(absolute(url), "utf8");

export const write = (url: string, file: string) =>
  writeFile(absolute(url), file, "utf8");

export const readYAML = <T>(name: string) =>
  read(name).then(load) as Promise<T>;

export const writeYAML = <T>(url: string, obj: T, ops?: DumpOptions) =>
  write(url, dump(obj, ops));

export class Yaml<T> {
  constructor(private f: (x: string) => string = (x) => x) {}
  read = (p: string) => readYAML<T>(this.f(p));
  write = (p: string, o: T, ops?: DumpOptions) => writeYAML(this.f(p), o, ops);
}
