import { push } from "./lib/utils/git";
import { ghApiREST, GH_ORG, GH_REPO } from "./lib/utils/gq-api";
import { exit } from "./lib/utils/utils";
import * as core from "@actions/core";
import { getChangelog } from "./lib/changelog";

import { Command } from "commander";
import { getVersion } from "./lib/utils/config";

const cli = new Command();

const handleError = (e: Error) => core.setFailed(e);

export const openRelease = (version: string, body: string) => {
  const branchName = `publish-release/${version}`;

  push(branchName);
  return ghApiREST(`repos/${GH_ORG}/${GH_REPO}/pulls`, {
    head: branchName,
    draft: true,
    base: "main",
    owner: GH_ORG,
    repo: GH_REPO,
    title: `Publish Release ${version}`,
    body,
  });
};

const draftRelease = async () => {
  const { body, next } = await getChangelog();
  core.setOutput("body", body);
  core.setOutput("version", next);
};

const describe = async () => {
  const version = await getVersion();
  core.setOutput("version", version);
};

cli.name("release").description("manage release").version("0.0.0");

cli
  .command("open")
  .description("open pull request for draft release")
  .argument("<string>", "version number")
  .option("-b, --body <string>", "pull request body", "")
  .action((version: string, { body }: { body: string }) =>
    openRelease(version, body).catch(exit)
  );

cli
  .command("draft")
  .description(
    `draft new release:
       - generates changelog
       - updates package versions
    `
  )
  .action(() => draftRelease().catch(handleError));

cli
  .command("describe")
  .description(`describe existing release`)
  .action(() => describe().catch(handleError));

cli.parse();
