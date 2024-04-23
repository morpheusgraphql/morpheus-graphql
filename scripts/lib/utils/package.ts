import { readYAML, writeYAML } from "./file";
import { Dict, PkgName, Version } from "./types";
import path from "path";

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

const FILE = "package.yaml";

export const getPackage = (dir: string) =>
  readYAML<StackPackage>(path.join(dir, FILE));

export const writePackage = (dir: string, pkg: StackPackage) =>
  writeYAML(path.join(dir, FILE), pkg);
