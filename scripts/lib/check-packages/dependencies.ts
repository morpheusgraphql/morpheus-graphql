import { log } from "../utils/utils";
import { formatTable } from "./formating";
import { Config } from "./types";

const PROJECT_PREFIX = "morpheus-graphql";

const withRule = (name: string, [min, max]: [string, string]) => [
  name,
  ">=",
  min,
  ...(max ? ["&&", "<", max] : []),
];

const updateDependency =
  ({ bounds, rules, allowUnknownLib }: Config) =>
  ([name, ...args]: string[]): string[] => {
    if (name.startsWith(PROJECT_PREFIX)) {
      if (!args.length) {
        return [name, ...args];
      }
      return withRule(name, bounds);
    }
    const rule = rules[name];

    if (rule) {
      return typeof rule === "boolean" ? [name] : withRule(name, rule);
    }

    if (allowUnknownLib) {
      log(` - unknown dependency: ${name}\n`, "warning");
      return [name];
    }

    throw new Error(`Unknown package: ${name}`);
  };

const formatDeps = (config: Config) => (dependencies: string[]) =>
  formatTable(
    dependencies
      .map((d) => d.split(/\s+/))
      .sort(([a], [b]) => a.charCodeAt(0) - b.charCodeAt(0))
      .map(updateDependency(config))
  );

export const updateDeps = (config: Config, value: unknown): unknown => {
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
        return [key, updateDeps(config, v)];
      })
    );
  }
  return value;
};
