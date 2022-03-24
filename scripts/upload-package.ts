import { dump, load } from "js-yaml";
import { promisify } from "util";
import { readFile, writeFile } from "fs";
import path from "path";
import { Config } from "./types";
import { formatDeps } from "./dependencies";

type StackPackage = {
  name: string;
  version: string;
  dependencies: string[];
};

const local = (p: string) => path.join(__dirname, "../", p);

const read = (url: string) => promisify(readFile)(local(url), "utf8");
const write = (url: string, file: string) =>
  promisify(writeFile)(local(url), file);

const recursiveDepUpdate = (config: Config, value: unknown): unknown => {
  if (!value) return value;
  if (typeof value === "object") {
    if (Array.isArray(value)) {
      return value.sort();
    }
    return Object.fromEntries(
      Object.entries(value).map(([key, v]) => {
        if (key === "dependencies") {
          return [key, formatDeps(config)(v)];
        }
        return [key, recursiveDepUpdate(config, v)];
      })
    );
  }
  return value;
};

const fixPackage = (config: Config, pkg: StackPackage): unknown =>
  recursiveDepUpdate(config, { ...pkg, version: config.version });

const checkPackage = async (name: string) => {
  const fileUrl = path.join(name, "package.yaml");
  const pkg = load(await read(fileUrl)) as StackPackage;

  const config = JSON.parse(await read("config.json"));
  const fixedPackage = fixPackage(config, pkg);
  await write(fileUrl, dump(fixedPackage));

  console.info(`Checked:\n  ${pkg.name}: ${pkg.version}`);
};

export const uploadPackage = async (name: string) => {
  const packageName = `morpheus-graphql${name ? `-${name}` : ""}`;
  await checkPackage(packageName);
};
