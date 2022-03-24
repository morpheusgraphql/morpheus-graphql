import { Config } from "./types";

const projectPefix = "morpheus-graphql";

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

const updateDependency =
  ({ bounds: [min, max] }: Config) =>
  ([name, ...args]: string[]): string[] => {
    if (name.startsWith(projectPefix) && args.length) {
      return [name, ">=", min, "&&", "<", max];
    }

    return [name, ...args];
  };

export const formatDeps = (config: Config) => (dependencies: string[]) =>
  formatMatrix(
    dependencies
      .map((d) => d.split(/\s+/))
      .sort(([a], [b]) => a.charCodeAt(0) - b.charCodeAt(0))
      .map(updateDependency(config))
  );
