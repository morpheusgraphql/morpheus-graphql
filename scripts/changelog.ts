import { exit } from "./lib/utils/utils";
import { write } from "./lib/utils/file";
import { getChangelog } from "./lib/changelog";

const main = async () => {
  const { body } = await getChangelog();

  await write("/dist/changelog.md", body);
  process.stdout.write(body);
};

main().catch(exit);
