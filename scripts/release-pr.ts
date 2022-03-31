import { push } from "./lib/utils/git";
import { ghApiREST, GH_ORG, GH_REPO } from "./lib/utils/gq-api";
import { exit } from "./lib/utils/utils";

const openRelease = (version: string) => {
  const branchName = `publish-release/${version}`;

  push(branchName);
  return ghApiREST(`repos/${GH_ORG}/${GH_REPO}/pulls`, {
    head: branchName,
    base: "master",
    owner: GH_ORG,
    repo: GH_REPO,
    title: `Publish Release ${version}`,
  });
};

openRelease(process.argv[2]).catch(exit);
