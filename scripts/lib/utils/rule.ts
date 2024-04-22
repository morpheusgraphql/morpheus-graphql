import { Bounds } from "../check-packages/types";

const RULE_SEPARATOR = "-";

export const parseRule = (s: string): Bounds => {
  const [min, max] = s.split(RULE_SEPARATOR);

  return [min, max];
};

export const formatRule = ([min, max]: Bounds) =>
  `${min}${RULE_SEPARATOR}${max}`;
