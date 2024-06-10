import { GHRelEasy } from "gh-rel-easy";
import { exec } from "./utils";
import { writeFile } from "fs/promises";
import { dirname, join } from "path";

export const hconf = async (
  cmd: "version" | "setup" | "next",
  ...ops: string[]
): Promise<string> => {
  const result = exec(["hconf", [cmd, ops].flat().join(" ")].join(" "));

  if (cmd !== "version") {
    console.log(result);
  }

  return Promise.resolve(result);
};

const write = (p: string) => (f: string) =>
  writeFile(join(dirname(require.main?.filename ?? ""), "../", p), f, "utf8");

const scope: Record<string, string> = {
  server: "morpheus-graphql",
  client: "morpheus-graphql-client",
  core: "morpheus-graphql-core",
  subscriptions: "morpheus-graphql-subscriptions",
  tests: "morpheus-graphql-tests",
  app: "morpheus-graphql-app",
};

const release = new GHRelEasy({
  pkg: (name) => `https://hackage.haskell.org/package/${scope[name]}`,
  gh: {
    org: "morpheusgraphql",
    repo: "morpheus-graphql",
  },
  scope,
  pr: {
    major: "Major Change",
    breaking: "Breaking Change",
    feature: "New features",
    fix: "Bug Fixes",
    chore: "Minor Changes",
  },
  version: () => hconf("version"),
  next: async (isBreaking) => {
    await hconf("next", ...(isBreaking ? ["-b"] : []));

    return hconf("version");
  },
});

export const open = (preview: boolean) =>
  release.changelog().then(async (body) => {
    await hconf("setup");

    if (preview) return;

    await release.open(body);
  });

export const changelog = () => release.changelog().then(write("changelog.md"));
