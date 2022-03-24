import { dump, load } from "js-yaml";
import { promisify } from "util";
import { exec } from "child_process";
import { readFile, writeFile } from "fs";
import path from "path";
import { Config } from "./types";
import { formatDeps } from "./dependencies";

type StackPackage = {
  name: string;
  version: string;
  dependencies: string[];
};

const local = path.join(__dirname, "../../../../");

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
  const fileUrl = path.join(local, name, "package.yaml");
  const file = await promisify(readFile)(fileUrl, "utf8");
  const pkg = load(file) as StackPackage;
  console.log(`package ${pkg.name}: ${pkg.version}`);

  const pkgYaml = dump(
    fixPackage(
      {
        bounds: ["0.19.0", "0.20.0"],
        version: "0.19.0",
      },
      pkg
    )
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
