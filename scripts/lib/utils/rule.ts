import { Rule } from "../check-packages/types";

const RULE_SEPARATOR = "-";

export const parseRule = (s: string): Rule => {
  if (s === "true") {
    return true;
  }

  const [min, max] = s.split(RULE_SEPARATOR);
  return [min, max];
};

export const formatRule = (rule: Rule) => {
  if (typeof rule === "boolean") return "true";

  const [min, max] = rule;

  return max ? `${min}${RULE_SEPARATOR}${max}` : min;
};
