import { dirname, join } from "path";
import { writeFile } from "fs/promises";
import { GHRelEasy } from "gh-rel-easy";
import { execSync, StdioOptions } from "child_process";

export const exit = (error: Error) => {
  console.log(error.message, "error");
  process.exit(1);
};

export const exec = (cmd: string, stdio?: StdioOptions) =>
  execSync(cmd, {
    maxBuffer: 10 * 1024 * 1024, // 10MB
    encoding: "utf-8",
    stdio,
  })?.trimEnd();

const hconf = async (
  cmd: "version" | "setup" | "next",
  ...ops: string[]
): Promise<string> => {
  const result = exec(["hconf", [cmd, ops].flat().join(" ")].join(" "));

  if (cmd !== "version") {
    console.log(result);
  }

  return Promise.resolve(result);
};

const version = () => hconf("version");

const next = (isBreaking: boolean) =>
  hconf("next", ...(isBreaking ? ["-b"] : [])).then(version);

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
  release.changelog().then(write("changelog.md")).catch(exit);
