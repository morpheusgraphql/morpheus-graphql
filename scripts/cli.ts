import { exit, hconf, write } from "./lib/utils";

import { Command } from "commander";
import { format } from "./lib/format";
import {  GHRelEasy } from "gh-rel-easy";

const cli = new Command();

cli.name("cli").description("cli").version("0.0.0");

const scope = {
  server: "morpheus-graphql",
  client: "morpheus-graphql-client",
  core: "morpheus-graphql-core",
  subscriptions: "morpheus-graphql-subscriptions",
  tests: "morpheus-graphql-tests",
  app: "morpheus-graphql-app",
};

const relEasy = new GHRelEasy({
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

cli
  .command("format")
  .description("format")
  .option("--fix <boolean>", "fix", false)
  .option("--path <string>", "path", "./morpheus-graphql*/**/*.hs")
  .action(format);

const release = cli.command("release");

release
  .command("open")
  .option("-p, --preview", "preview", false)
  .action(({ preview }: { preview: string }) =>
    relEasy
      .changelog()
      .then(async (body) => {
        await hconf("setup");

        if (preview) return;

        await relEasy.open(body);
      })
      .catch(exit)
  );

release
  .command("changelog")
  .action(() => relEasy.changelog().then(write("changelog.md")).catch(exit));

cli.parse();
