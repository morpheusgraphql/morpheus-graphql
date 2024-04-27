export type VersionUpdate = {
  prev: string;
  next: string;
  isBreaking: boolean;
};

export type StrVersion = string;

type Version = [number, number, number];

const toVersion = (x: string): Version =>
  x === "latest" ? [Infinity, 0, 0] : parse(x);

export const compare = ([x, ...xs]: number[], [y, ...ys]: number[]): number => {
  if (x === undefined && y == undefined) {
    return 0;
  }

  if (x === y) {
    return compare(xs, ys);
  }

  return x - y;
};

const parse = (versionTag: string): Version => {
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
    this.v = typeof v === "string" ? parse(v) : v;
  }

  up(isBreaking: boolean) {
    return new ParsedVersion(genVersion(this.v, isBreaking));
  }

  format = () => this.v.join(".");

  static compare = (x: string, y: string) =>
    compare(toVersion(x), toVersion(y));
}
