import { Command } from "commander";
import { format } from "./local/format";
import { setup } from "./local/setup";

const cli = new Command();

cli.name("local").version("0.0.0");

cli
  .command("setup")
  .description("setup stack config")
  .argument("[string]", "version number", "latest")
  .action(setup);

cli
  .command("format")
  .description("format")
  .option("--fix <boolean>", "fix", false)
  .option("--path <string>", "path", "./morpheus-graphql*/**/*.hs")
  .action(format);

cli.parse();
