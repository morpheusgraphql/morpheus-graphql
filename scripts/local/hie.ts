import { join } from "path";
import { StackPackage } from "../lib/check-packages/types";
import { readYAML, writeYAML } from "../lib/utils/file";
import { log } from "../lib/utils/utils";

type Pkg = { path: string } & StackPackage;

const getPath = (path: string, src: StackPackage["library"]) =>
  "./" + join(path, src["source-dirs"]);

const scanSub = (
  configs: Pkg[],
  type: string,
  key: "tests" | "executables" | "benchmarks"
) =>
  configs.flatMap((pkg) =>
    Object.entries(pkg[key] ?? {}).map(([sub, lib]) => ({
      path: getPath(pkg.path, lib),
      component: [pkg.name, type, sub].join(":"),
    }))
  );

const scanLib = (configs: Pkg[]) =>
  configs.flatMap(({ path, name, library }) =>
    library
      ? [
          {
            path: getPath(path, library),
            component: `${name}:lib`,
          },
        ]
      : []
  );

export const hie = async (sources: string[]) => {
  const packages = await Promise.all(
    sources.map(async (name) => {
      return {
        ...(await readYAML<StackPackage>(join(name, "package.yaml"))),
        path: name,
      };
    })
  );

  writeYAML("hie.yaml", {
    cradle: {
      stack: {
        stackYaml: "./stack.yaml",
        components: [
          ...scanLib(packages),
          ...scanSub(packages, "test", "tests"),
          ...scanSub(packages, "exe", "executables"),
          ...scanSub(packages, "bench", "benchmarks"),
        ],
      },
    },
  });

  log("generated hie.yaml\n\n", "success");
};
