import { readYAML } from "./file";
import { Dict, PkgName, Version } from "./types";

type Src = { "source-dirs": string };

export type StackPackage = {
  name: PkgName;
  version: Version;
  dependencies: PkgName[];
  library: Src;
  tests: Dict<Src>;
  executables: Dict<Src>;
  benchmarks: Dict<Src>;
};

export const getPackage = (name: string) => readYAML<StackPackage>(name);
