export type Version = string;

export type PkgName = string;

export type Bounds<R extends boolean = false> = R extends true
  ? string
  : [Version, Version];

export type Rule<R extends boolean = false> = true | Bounds<R>;

export type Rules<Raw extends boolean = false> = Record<PkgName, Rule<Raw>>;

type Dict<T> = Record<string, T>;

export type StackPlan = {
  deps?: Dict<PkgName>;
  resolver: string;
  include?: PkgName[];
  skip?: PkgName[];
};

export type Config<R extends boolean = false> = {
  version: Version;
  bounds: Bounds<R>;
  rules: Rules<R>;
  packages: PkgName[];
  plan: Dict<StackPlan>;
  examples: PkgName[];
};

export type Table = string[][];

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
