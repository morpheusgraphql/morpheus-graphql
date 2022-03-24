export type VersionNumber = string;

export type Config = {
  version: VersionNumber;
  bounds: [VersionNumber, VersionNumber];
};
