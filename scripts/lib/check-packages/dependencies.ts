import { formatTable } from "./formatting";
import { Config } from "../utils/config";
import { StackPackage } from "../utils/package";

const formatDeps = <T extends string[]>(config: Config, deps: T) =>
  formatTable(
    deps
      .map((d) => d.split(/\s+/))
      .sort(([a], [b]) => a.charCodeAt(0) - b.charCodeAt(0))
      .map(([name, ...args]) => config.checkDependency(name, !args.length))
  ) as T;

const update = <T>(config: Config, value: T, k?: string): T => {
  if (!value) return value;
  if (typeof value === "object") {
    if (Array.isArray(value)) {
      return k === "dependencies"
        ? formatDeps(config, value.sort())
        : value.sort();
    }

    Object.fromEntries(
      Object.entries(value).map(([k, v]) => [k, update(config, v, k)])
    ) as T;
  }
  return value;
};

export const updatePackage = (config: Config, pkg: StackPackage) =>
  update(config, { ...pkg, version: config.version });
