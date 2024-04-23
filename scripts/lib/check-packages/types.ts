export type Version = string;

export type PackageName = string;

export type Bounds<R extends boolean = false> = R extends true
  ? string
  : [Version, Version];

export type Rule<R extends boolean = false> = true | Bounds<R>;

export type Rules<Raw extends boolean = false> = Record<PackageName, Rule<Raw>>;

type Dict<T> = Record<string, T>;

export type StackPlan = {
  deps?: Dict<PackageName>;
  resolver: string;
  include?: PackageName[];
  skip?: PackageName[];
};

export type Config<R extends boolean = false> = {
  version: Version;
  bounds: Bounds<R>;
  rules: Rules<R>;
  packages: PackageName[];
  plan: Dict<StackPlan>;
  examples: PackageName[];
};

export type Table = string[][];

type Src = { "source-dirs": string };

export type StackPackage = {
  name: PackageName;
  version: Version;
  dependencies: PackageName[];
  library: Src;
  tests: Dict<Src>;
  executables: Dict<Src>;
  benchmarks: Dict<Src>;
};
