import { push } from "./lib/utils/git";
import { ghApiREST, GH_ORG, GH_REPO } from "./lib/utils/gq-api";
import { exit } from "./lib/utils/utils";
// import { exit } from "./lib/utils/utils";
const { Command } = require("commander");
const cli = new Command();

export const openRelease = (version: string, body: string) => {
  const branchName = `publish-release/${version}`;

  push(branchName);
  return ghApiREST(`repos/${GH_ORG}/${GH_REPO}/pulls`, {
    head: branchName,
    base: "master",
    owner: GH_ORG,
    repo: GH_REPO,
    title: `Publish Release ${version}`,
    body,
  });
};

cli.name("release").description("manage release").version("0.0.0");

cli
  .command("pr")
  .description("open release pull request")
  .argument("<string>", "version number")
  .option("-b, --body <string>", "pull request body", "")
  .action((version: string, { body }: { body: string }) =>
    openRelease(version, body).catch(exit)
  );

cli.parse();
