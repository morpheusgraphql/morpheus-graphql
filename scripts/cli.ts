import { Command } from "commander";
import { open, changelog } from "./release";

const cli = new Command();

cli.name("cli").description("cli").version("0.0.0");

cli.command("open").option("-p, --preview", "preview", false).action(open);

cli.command("changelog").action(changelog);

cli.parse();
