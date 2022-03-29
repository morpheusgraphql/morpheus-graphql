export type VersionNumber = string;

export type Bounds = [VersionNumber, VersionNumber];
export type PackageName = string;

export type Config = {
  version: VersionNumber;
  bounds: Bounds;
  rules: Record<PackageName, Bounds>;
  packages: PackageName[];
};

export type Table = string[][];

export type StackPackage = {
  name: string;
  version: string;
  dependencies: string[];
};
