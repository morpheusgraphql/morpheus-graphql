export type VersionUpdate = {
  prev: string;
  next: string;
  isBreaking: boolean;
};

export type StrVersion = string;

type Vector = [number, number, number];

const SEPARATOR = ".";

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
}
