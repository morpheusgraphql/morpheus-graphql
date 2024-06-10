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

cli.command("open").option("-p, --preview", "preview", false).action(open);

cli.command("changelog").action(changelog);

cli.parse();
