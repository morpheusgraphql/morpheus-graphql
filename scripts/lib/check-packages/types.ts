export type VersionNumber = string;

export type DepsMap = Record<string, Bounds>;

export type Bounds = [VersionNumber, VersionNumber];

export type PackageName = string;

export type StackPlan = {
  deps?: Record<string, string>;
  resolver: string;
  include?: string[];
  skip?: string[];
};

export type Config = {
  version: VersionNumber;
  bounds: Bounds;
  rules: Record<PackageName, Bounds>;
  packages: PackageName[];
  plan: Record<string, StackPlan>;
  examples: string[];
};

export type Table = string[][];

type Src = {
  "source-dirs": string;
};

export type StackPackage = {
  name: string;
  version: string;
  dependencies: string[];
  library: Src;
  tests: Record<string, Src>;
  executables: Record<string, Src>;
  benchmarks: Record<string, Src>;
};
