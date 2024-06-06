import { github } from "./lib/gh";
import { exit, write } from "./lib/utils";
import { changelog } from "./lib/changelog";

import { Command } from "commander";
import { format } from "./lib/format";

const cli = new Command();

cli.name("cli").description("cli").version("0.0.0");

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
    changelog(true)
      .then(preview ? () => Promise.resolve() : github.release)
      .catch(exit)
  );

release
  .command("changelog")
  .action(() => changelog().then(write("changelog.md")).catch(exit));

cli.parse();
