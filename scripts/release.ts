import { writeFile } from "fs/promises";
import { GHRelEasy } from "gh-rel-easy";
import { execSync } from "child_process";

export const exit = (error: Error) => {
  console.log(error.message, "error");
  process.exit(1);
};

const hconf = async (
  cmd: "version" | "setup" | "next",
  ...ops: string[]
): Promise<string> => {
  const result = execSync(["hconf", [cmd, ops].flat().join(" ")].join(" "), {
    maxBuffer: 10 * 1024 * 1024,
    encoding: "utf-8",
  })?.trimEnd();

  if (cmd !== "version") {
    console.log(result);
  }

  return Promise.resolve(result);
};

const version = () => hconf("version");

const next = (isBreaking: boolean) =>
  hconf("next", ...(isBreaking ? ["-b"] : [])).then(version);

const scope: Record<string, string> = {
  server: "morpheus-graphql",
  client: "morpheus-graphql-client",
  core: "morpheus-graphql-core",
  subscriptions: "morpheus-graphql-subscriptions",
  tests: "morpheus-graphql-tests",
  app: "morpheus-graphql-app",
};

const pkg = (name: string) =>
  `https://hackage.haskell.org/package/${scope[name]}`;

const release = new GHRelEasy({
  gh: {
    org: "morpheusgraphql",
    repo: "morpheus-graphql",
  },
  pr: {
    major: "Major Change",
    breaking: "Breaking Change",
    feature: "New features",
    fix: "Bug Fixes",
    chore: "Minor Changes",
  },
  scope,
  version,
  next,
  pkg,
});

export const open = ({ preview }: { preview: boolean }) =>
  release
    .changelog()
    .then((body) =>
      hconf("setup").then(() => (preview ? undefined : release.open(body)))
    )
    .catch(exit);

export const changelog = () =>
  release
    .changelog()
    .then((body: string) => writeFile("../changelog.md", body, "utf8"))
    .catch(exit);
