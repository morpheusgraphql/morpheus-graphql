import { Yaml } from "./file";
import { Dict, PkgName } from "./types";
import path from "path";
import { StrVersion } from "./version";

type Src = { "source-dirs": string };

export type StackPackage = {
  name: PkgName;
  version: StrVersion;
  dependencies: PkgName[];
  library: Src;
  tests: Dict<Src>;
  executables: Dict<Src>;
  benchmarks: Dict<Src>;
};

export const Package = new Yaml<StackPackage>((dir) =>
  path.join(dir, "package.yaml")
);
