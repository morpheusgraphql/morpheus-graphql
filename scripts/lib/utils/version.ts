export type VersionUpdate = {
  prev: string;
  next: string;
  isBreaking: boolean;
};

type Version = [number, number, number];

export const isHigherOrEQ = (
  [x, ...xs]: number[],
  [y, ...ys]: number[]
): boolean => {
  if (x === undefined && y == undefined) {
    return true;
  }

  return x > y || (x === y && isHigherOrEQ(xs, ys));
};

const parseVersion = (versionTag: string): Version => {
  const vs = versionTag.split(".").map((v) => parseInt(v, 10));

  if (vs.length !== 3 || vs.find(isNaN) !== undefined) {
    throw new Error(`Invalid verion : ${versionTag}`);
  }

  const [major, minor, revision] = vs;

  return [major, minor, revision];
};

const genVersion = ([major, minor, revision]: Version, isBreaking: boolean) =>
  isBreaking ? [major, minor + 1, 0] : [major, minor, revision + 1];

export { parseVersion, genVersion };
