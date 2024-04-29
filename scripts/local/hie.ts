import { join } from "path";
import { Yaml } from "../lib/utils/file";
import { Package, StackPackage } from "../lib/utils/package";
import { defs } from "../lib/utils/defs";

type Pkg = { path: string } & StackPackage;

const Hie = new Yaml<unknown, []>(() => defs.HIE);

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
    sources.map(async (dir) => {
      return {
        ...(await Package.read(dir)),
        path: dir,
      };
    })
  );

  Hie.write({
    cradle: {
      stack: {
        stackYaml: defs.STACK,
        components: [
          ...scanLib(packages),
          ...scanSub(packages, "test", "tests"),
          ...scanSub(packages, "exe", "executables"),
          ...scanSub(packages, "bench", "benchmarks"),
        ],
      },
    },
  });
};
