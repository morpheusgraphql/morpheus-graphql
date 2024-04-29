import { formatTable } from "./formatting";
import { Config } from "../utils/config";
import { StackPackage } from "../utils/package";

const formatDeps = (config: Config) => (dependencies: string[]) =>
  formatTable(
    dependencies
      .map((d) => d.split(/\s+/))
      .sort(([a], [b]) => a.charCodeAt(0) - b.charCodeAt(0))
      .map(([name, ...args]) => config.checkDependency(name, !args.length))
  );

export const updateObjectDeps = <T extends object>(config: Config, value: T) =>
  Object.fromEntries(
    Object.entries(value).map(([k, v]) => {
      return [
        k,
        k === "dependencies" ? formatDeps(config)(v) : updateDeps(config, v),
      ];
    })
  ) as T;

const updateDeps = <T extends object>(config: Config, value: T): T => {
  if (!value) return value;
  if (typeof value === "object") {
    if (Array.isArray(value)) {
      return value.sort();
    }

    return updateObjectDeps(config, value);
  }
  return value;
};

export const updatePackage = (config: Config, pkg: StackPackage) =>
  updateDeps(config, { ...pkg, version: config.version });
