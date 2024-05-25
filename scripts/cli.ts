import { github } from "./lib/gh";
import { exit, hconf, write } from "./lib/utils";
import * as core from "@actions/core";
import { getChangelog } from "./lib/changelog";

import { Command } from "commander";
import { format } from "./lib/format";

const cli = new Command();

const handleError = (e: Error) => core.setFailed(e);

const draftRelease = async () =>
  core.setOutput("body", await getChangelog(true));

const openRelease = async (body: string) => {
  const v = hconf("version");
  await github.openPR(`publish-release/${v}`, `Publish Release ${v}`, body);
};

const changelog = async () =>
  await write("/changelog.md", await getChangelog());

cli.name("cli").description("cli").version("0.0.0");

const release = cli.command("release");

release
  .command("open")
  .description("open pull request for draft release")
  .option("-b, --body <string>", "pull request body", "")
  .action(({ body }: { body: string }) => openRelease(body).catch(exit));

release
  .command("draft")
  .description(
    `draft new release:
       - generates changelog
       - updates package versions
    `
  )
  .action(() => draftRelease().catch(handleError));

release
  .command("describe")
  .description(`describe existing release`)
  .action(() => core.setOutput("version", hconf("version")));

release.command("changelog").action(() => changelog().catch(handleError));

cli
  .command("format")
  .description("format")
  .option("--fix <boolean>", "fix", false)
  .option("--path <string>", "path", "./morpheus-graphql*/**/*.hs")
  .action(format);

cli.parse();
