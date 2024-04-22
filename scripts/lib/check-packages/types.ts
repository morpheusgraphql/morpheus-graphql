export type VersionNumber = string;

export type PackageName = string;

export type Rule<Raw extends boolean = false> = Raw extends true
  ? string | true
  : Bounds | true;

export type Rules<Raw extends boolean = false> = Record<PackageName, Rule<Raw>>;

export type Bounds = [VersionNumber, VersionNumber];

export type StackPlan = {
  deps?: Record<string, string>;
  resolver: string;
  include?: string[];
  skip?: string[];
};

export type Config<R extends boolean = false> = {
  version: VersionNumber;
  bounds: R extends true ? string : Bounds;
  rules: Rules<R>;
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
