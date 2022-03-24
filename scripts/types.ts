export type VersionNumber = string;

export type Bounds = [VersionNumber, VersionNumber];
export type PackageName = string;

export type Config = {
  version: VersionNumber;
  bounds: Bounds;
  rules: Record<PackageName, Bounds>;
};

export type Table = string[][];
