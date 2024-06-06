import { github } from "./lib/gh";
import { exit, write } from "./lib/utils";
import { changelog } from "./lib/changelog";

import { Command } from "commander";
import { format } from "./lib/format";
import { Config } from "./lib/changelog/types";

const cli = new Command();

cli.name("cli").description("cli").version("0.0.0");

const config: Config = {
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
    changelog(config, true)
      .then(preview ? () => Promise.resolve() : github.release)
      .catch(exit)
  );

release
  .command("changelog")
  .action(() => changelog(config).then(write("changelog.md")).catch(exit));

cli.parse();
