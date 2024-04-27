export type VersionUpdate = {
  prev: string;
  next: string;
  isBreaking: boolean;
};

export type StrVersion = string;

type VersionTuple = [number, number, number];

const toVersion = (x: string): VersionTuple =>
  x === "latest" ? [Infinity, 0, 0] : parse(x);

const compare = ([x, ...xs]: number[], [y, ...ys]: number[]): number => {
  if (x === undefined && y == undefined) {
    return 0;
  }

  if (x === y) {
    return compare(xs, ys);
  }

  return x - y;
};

const parse = (versionTag: string): VersionTuple => {
  const vs = versionTag.split(".").map((v) => parseInt(v, 10));

  if (vs.length !== 3 || vs.find(isNaN) !== undefined) {
    throw new Error(`invalid version : ${versionTag}`);
  }

  const [major, minor, revision] = vs;

  return [major, minor, revision];
};

export class Version {
  private tuple: VersionTuple;

  constructor(v: string | VersionTuple) {
    this.tuple = typeof v === "string" ? parse(v) : v;
  }

  next(isBreaking: boolean) {
    const [major, minor, revision] = this.tuple;

    return new Version(
      isBreaking ? [major, minor + 1, 0] : [major, minor, revision + 1]
    );
  }

  format = () => this.tuple.join(".");

  static compare = (x: string, y: string) =>
    compare(toVersion(x), toVersion(y));
}
