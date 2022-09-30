import { exit, stdout } from "process";
import { exec } from "./lib/utils/utils";
import { Command } from "commander";
import { promisify } from "util";
import glob from "glob";

const cli = new Command();

const root = "./examples/code-gen/src";

const format = async () => {
  try {
    exec("stack install --fast --test morpheus-graphql-code-gen");

    const files = (await promisify(glob)(`${root}/**/*.gql`)).join(" ");

    exec(`morpheus --root ${root} build ${files}`);

    stdout.write("OK");
  } catch (e) {
    stdout.write(e.message);
    exit(1);
  }
};

cli
  .name("cod-gen samples")
  .description("generates Morpheus code based on samples")
  .version("0.0.0");
cli.action(format);

cli.parse();
