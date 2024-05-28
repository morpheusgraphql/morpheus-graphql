import { github } from "./lib/gh";
import { exit, hconf, write } from "./lib/utils";
import * as core from "@actions/core";
import { changelog } from "./lib/changelog";

import { Command } from "commander";
import { format } from "./lib/format";

const cli = new Command();

const handleError = (e: Error) => core.setFailed(e);

cli.name("cli").description("cli").version("0.0.0");

const release = cli.command("release");

release
  .command("open")
  .description("open pull request for draft release")
  .option("-b, --body <string>", "pull request body", "")
  .action(({ body }: { body: string }) =>
    hconf("version")
      .then((v) =>
        github.openPR(`publish-release/${v}`, `Publish Release ${v}`, body)
      )
      .catch(exit)
  );

release
  .command("draft")
  .description(
    `draft new release:
       - generates changelog
       - updates package versions
    `
  )
  .action(() =>
    changelog(true)
      .then((body) => core.setOutput("body", body))
      .catch(handleError)
  );

release
  .command("describe")
  .description(`describe existing release`)
  .action(() => hconf("version").then((v) => core.setOutput("version", v)));

release.command("changelog").action(() =>
  changelog()
    .then((file) => write("/changelog.md", file))
    .catch(handleError)
);

cli
  .command("format")
  .description("format")
  .option("--fix <boolean>", "fix", false)
  .option("--path <string>", "path", "./morpheus-graphql*/**/*.hs")
  .action(format);

cli.parse();
