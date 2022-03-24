import { dump, load } from "js-yaml";
import { promisify } from "util";
import { exec } from "child_process";
import { readFile, writeFile } from "fs";
import path from "path";

type StackPackage = {
  name: string;
  version: string;
  dependencies: string[];
};

const projectPefix = "morpheus-graphql";

const local = path.join(__dirname, "../../../../");
const version = {
  min: "0.19.0",
  max: "0.20.0",
  current: "0.19.0",
};

type Matrix = string[][];

const genGap = (item: number, max: number) =>
  Array.from({ length: max - item }, () => " ").join("");

const getSize = (mx: Matrix, index: number) =>
  Math.max(...mx.map((xs) => xs[index]?.length ?? 0));

const formatMatrix = (xs: Matrix) =>
  xs.map((row) =>
    row
      .reduce(
        (txt, item, i) =>
          txt + item + genGap(item.length, getSize(xs, i)) + "  ",
        ""
      )
      .trim()
  );

const updateDependency = ([name, ...args]: string[]): string[] => {
  if (name.startsWith(projectPefix) && args.length) {
    return [name, ">=", version.min, "&&", "<", version.max];
  }

  return [name, ...args];
};

const formatDeps = (dependencies: string[]) =>
  formatMatrix(
    dependencies
      .map((d) => d.split(/\s+/))
      .sort(([a], [b]) => a.charCodeAt(0) - b.charCodeAt(0))
      .map(updateDependency)
  );

const recursiveDepUpdate = (value: unknown): unknown => {
  if (!value) return value;
  if (typeof value === "object") {
    if (Array.isArray(value)) {
      return value;
    }
    return Object.fromEntries(
      Object.entries(value).map(([key, v]) => {
        if (key === "dependencies") {
          return [key, formatDeps(v)];
        }
        return [key, recursiveDepUpdate(v)];
      })
    );
  }
  return value;
};

const checkPackage = async (name: string) => {
  const fileUrl = path.join(local, name, "package.yaml");
  const file = await promisify(readFile)(fileUrl, "utf8");
  const pkg = load(file) as StackPackage;

  console.log(`package ${pkg.name}: ${pkg.version}`);

  const pkgYaml = dump(
    recursiveDepUpdate({
      ...pkg,
      version: version.current,
    })
  );

  await promisify(writeFile)(fileUrl, pkgYaml);
};

export const uploadPackage = async (name: string) => {
  const packageName = `morpheus-graphql${name ? `-${name}` : ""}`;

  console.info(`start uploading package ${packageName}!`);

  // check package version number
  await checkPackage(packageName);

  // upload package
  await promisify(exec)(`stack upload ${packageName}`);

  console.info(`successfully uploaded package${packageName}!`);
};
