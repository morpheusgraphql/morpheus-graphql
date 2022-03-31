import * as core from "@actions/core";
import { getChangelog } from "./lib/changelog";
import { checkPackages } from "./lib/check-packages";

const prepareRelease = async () => {
  const { body, version } = await getChangelog();

  await checkPackages(version);
  core.setOutput("body", body);
  core.setOutput("version", version.next);
};

prepareRelease().catch((e) => core.setFailed(e));
