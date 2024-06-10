import { exit } from "./utils";
import { Command } from "commander";
import { format } from "./format";
import { open, changelog } from "./release";

const cli = new Command();

cli.name("cli").description("cli").version("0.0.0");

cli
  .command("format")
  .description("format")
  .option("--fix <boolean>", "fix", false)
  .option("--path <string>", "path", "./morpheus-graphql*/**/*.hs")
  .action(format);

cli
  .command("open")
  .option("-p, --preview", "preview", false)
  .action(({ preview }: { preview: boolean }) => open(preview).catch(exit));

cli.command("changelog").action(() => changelog().catch(exit));

cli.parse();
