import { Maybe } from "../types";

const config = {
  scope: {
    server: "morpheus-graphql",
    client: "morpheus-graphql-client",
    core: "morpheus-graphql-core",
    subscriptions: "morpheus-graphql-subscriptions",
    tests: "morpheus-graphql-tests",
    app: "morpheus-graphql-app",
  },
  pr: {
    breaking: "Breaking Change",
    feature: "New features",
    fix: "Bug Fixes",
    chore: "Minor Changes",
  },
};

export const pullRequestTypes = config.pr;

type CONFIG = typeof config;
type LabelKind = keyof CONFIG;
type VALUES<T extends LabelKind> = keyof CONFIG[T];

export const parseLabel =
  <T extends LabelKind>(kind: T) =>
  (label: string): Maybe<VALUES<T>> => {
    const [prefix, name, ...rest] = label.split("/");

    if (prefix !== kind) return;

    if (rest.length || !name || !(config[kind] as any)[name]) {
      throw new Error(`invalid label ${label}`);
    }

    return name as VALUES<T>;
  };

type SCOPE = keyof typeof config.scope;

type PR_TYPE = keyof typeof pullRequestTypes;

export { SCOPE, PR_TYPE, config };
