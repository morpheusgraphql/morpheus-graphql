import { Bounds, Rule } from "../check-packages/types";

const RULE_SEPARATOR = "-";

export const parseBound = (s: string): Bounds => {
  const [min, max] = s.split(RULE_SEPARATOR);
  return [min, max];
};

export const parseRule = (s: string | true): Rule =>
  s === true ? true : parseBound(s);

export const formatRule = (rule: Rule) => {
  if (typeof rule === "boolean") return true;

  const [min, max] = rule;

  return max ? `${min}${RULE_SEPARATOR}${max}` : min;
};
