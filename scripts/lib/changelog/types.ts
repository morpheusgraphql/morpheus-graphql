import { Maybe } from "../types";

type PRType = "breaking" | "feature" | "fix" | "chore";

type Config = {
  scope: Record<string, string>;
  pr: Record<PRType, string>;
};

type LabelKind = keyof Config;

type Value<T extends LabelKind> = keyof Config[T];

const parseLabel =
  <T extends LabelKind>(config: Config, kind: T) =>
  (label: string): Maybe<Value<T>> => {
    const [prefix, name, ...rest] = label.split("/");

    if (prefix !== kind) return;

    if (rest.length || !name || !(config[kind] as any)[name]) {
      throw new Error(`invalid label ${label}`);
    }

    return name as Value<T>;
  };

const parseLabels = <T extends LabelKind>(
  config: Config,
  kind: T,
  labels: string[]
) =>
  labels
    .map(parseLabel(config, kind))
    .filter((x: Value<T> | undefined): x is Value<T> => Boolean(x));

export { PRType, Config, parseLabels };
