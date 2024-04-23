export type Version = string;

export type Maybe<T> = undefined | T;

export type PkgName = string;

export type Bounds<R extends boolean = false> = R extends true
  ? string
  : [Version, Version];

export type Rule<R extends boolean = false> = true | Bounds<R>;

export type Rules<Raw extends boolean = false> = Record<PkgName, Rule<Raw>>;

export type Dict<T> = Record<string, T>;

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
