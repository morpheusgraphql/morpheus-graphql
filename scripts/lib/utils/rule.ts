import { PkgName } from "./types";
import { StrVersion } from "./version";

export type Bounds<R extends boolean = false> = R extends true
  ? string
  : [StrVersion, StrVersion];

export type Rule<R extends boolean = false> = true | Bounds<R>;

export type Rules<Raw extends boolean = false> = Record<PkgName, Rule<Raw>>;

const RULE_SEPARATOR = "-";

export const parseBound = (s: Bounds<true>): Bounds => {
  const [min, max] = s.split(RULE_SEPARATOR);
  return [min, max];
};

export const parseRule = (s: Rule<true>): Rule =>
  s === true ? true : parseBound(s);

export const formatRule = (rule: Rule): Rule<true> => {
  if (typeof rule === "boolean") return true;

  const [min, max] = rule;

  return max ? `${min}${RULE_SEPARATOR}${max}` : min;
};

export const withRule = (name: string, [min, max]: [string, string]) => [
  name,
  ">=",
  min,
  ...(max ? ["&&", "<", max] : []),
];
