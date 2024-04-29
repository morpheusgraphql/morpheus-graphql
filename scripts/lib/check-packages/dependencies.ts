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

const updateDeps = <T extends object>(
  config: Config,
  value: T,
  isDeps?: boolean
): T => {
  if (!value) return value;
  if (typeof value === "object") {
    if (Array.isArray(value)) {
      return isDeps ? formatDeps(config, value.sort()) : value.sort();
    }

    Object.fromEntries(
      Object.entries(value).map(([k, v]) => {
        return [k, updateDeps(config, v, k === "dependencies")];
      })
    ) as T;
  }
  return value;
};

export const updatePackage = (config: Config, pkg: StackPackage) =>
  updateDeps(config, { ...pkg, version: config.version });
