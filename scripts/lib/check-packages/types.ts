export type VersionNumber = string;

export type PackageName = string;

export type Rules<T = Bounds> = Record<PackageName, T>;

export type Bounds = [VersionNumber, VersionNumber];

export type StackPlan = {
  deps?: Record<string, string>;
  resolver: string;
  include?: string[];
  skip?: string[];
};

export type Config<T = Bounds> = {
  version: VersionNumber;
  bounds: Bounds;
  rules: Rules<T>;
  packages: PackageName[];
  plan: Record<string, StackPlan>;
  examples: string[];
  allowUnknownLib: boolean;
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
