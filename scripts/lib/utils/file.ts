import { readFile, writeFile } from "fs/promises";
import { dirname, join } from "path";
import { DumpOptions, dump, load } from "js-yaml";

const ROOT = join(dirname(require.main?.filename ?? ""), "../");

const absolute = (p: string) => join(ROOT, p);

export const read = (url: string) => readFile(absolute(url), "utf8");

export const write = (url: string, file: string) =>
  writeFile(absolute(url), file, "utf8");

export class Yaml<T, A extends string[] = [string]> {
  constructor(private file: (...x: A) => string) {}
  read = (...p: A) => read(this.file(...p)).then(load) as Promise<T>;
  write = (o: T, ops?: DumpOptions, ...p: A) =>
    write(this.file(...p), dump(o, ops));
}
