export type VersionUpdate = {
  prev: string;
  next: string;
  isBreaking: boolean;
};

export type StrVersion = string;

type Version = [number, number, number];

export const compareVersion = (x: string, y: string) =>
  compareSeries(parseX(x), parseX(y));

const parseX = (x: string): Version =>
  x === "latest" ? [Infinity, 0, 0] : parseVersion(x);

export const compareSeries = (
  [x, ...xs]: number[],
  [y, ...ys]: number[]
): number => {
  if (x === undefined && y == undefined) {
    return 0;
  }

  if (x === y) {
    return compareSeries(xs, ys);
  }

  return x - y;
};

const parseVersion = (versionTag: string): Version => {
  const vs = versionTag.split(".").map((v) => parseInt(v, 10));

  if (vs.length !== 3 || vs.find(isNaN) !== undefined) {
    throw new Error(`invalid version : ${versionTag}`);
  }

  const [major, minor, revision] = vs;

  return [major, minor, revision];
};

const genVersion = (
  [major, minor, revision]: Version,
  isBreaking: boolean
): Version =>
  isBreaking ? [major, minor + 1, 0] : [major, minor, revision + 1];

export class ParsedVersion {
  private v: Version;

  constructor(v: string | Version) {
    this.v = typeof v === "string" ? parseVersion(v) : v;
  }

  up(isBreaking: boolean) {
    return new ParsedVersion(genVersion(this.v, isBreaking));
  }

  format = () => this.v.join(".");
}
