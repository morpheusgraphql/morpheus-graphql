import { formatTable } from "./formating";
import { Config } from "./types";

const projectPefix = "morpheus-graphql";

const withRule = (name: string, [min, max]: [string, string]) => [
  name,
  ">=",
  min,
  "&&",
  "<",
  max,
];

const updateDependency =
  ({ bounds, rules }: Config) =>
  ([name, ...args]: string[]): string[] => {
    if (name.startsWith(projectPefix)) {
      if (!args.length) {
        return [name, ...args];
      }
      return withRule(name, bounds);
    }
    const rule = rules[name];

    if (rule) {
      return withRule(name, rule);
    }

    throw new Error(`Unknown package: ${name}`);
  };

export const formatDeps = (config: Config) => (dependencies: string[]) =>
  formatTable(
    dependencies
      .map((d) => d.split(/\s+/))
      .sort(([a], [b]) => a.charCodeAt(0) - b.charCodeAt(0))
      .map(updateDependency(config))
  );
