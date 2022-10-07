import { Command } from "commander";
import { codeGen } from "./local/code-gen";
import { format } from "./local/format";
import { setup } from "./local/setup";

const cli = new Command();

cli.name("local").version("0.0.0");

cli
  .command("setup")
  .description("setup stack config")
  .argument("<string>", "version number")
  .action(setup);

cli
  .command("code-gen")
  .description("generates Morpheus code based on samples")
  .action(codeGen);

cli
  .command("format")
  .description("format")
  .option("--fix <boolean>", "fix", false)
  .option("--path <string>", "path", "./morpheus-graphql*/**/*.hs")
  .action(format);

cli.parse();
