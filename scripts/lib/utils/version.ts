export type VersionUpdate = {
  prev: string;
  next: string;
  isBreaking: boolean;
};

export type StrVersion = string;

type Vector = [number, number, number];

const SEPARATOR = ".";

const toVersion = (v: string): Vector =>
  v === "latest" ? [Infinity, 0, 0] : parse(v);

const compare = ([x, ...xs]: number[], [y, ...ys]: number[]): number => {
  if (x === undefined && y == undefined) {
    return 0;
  }

  if (x === y) {
    return compare(xs, ys);
  }

  return x - y;
};

const parse = (tag: string): Vector => {
  const vs = tag.split(SEPARATOR).map((s) => parseInt(s, 10));
  const [major, minor, revision] = vs;

  if (vs.length !== 3 || vs.find(isNaN) !== undefined) {
    throw new Error(`invalid version: ${tag}`);
  }

  return [major, minor, revision];
};

export class Version {
  private tuple: Vector;

  constructor(v: string | Vector) {
    this.tuple = typeof v === "string" ? parse(v) : v;
  }

  public next = (isBreaking: boolean) => {
    const [major, minor, revision] = this.tuple;

    return new Version(
      isBreaking ? [major, minor + 1, 0] : [major, minor, revision + 1]
    );
  };

  public format = () => this.tuple.join(SEPARATOR);

  static compare = (x: string, y: string) =>
    compare(toVersion(x), toVersion(y));
}
