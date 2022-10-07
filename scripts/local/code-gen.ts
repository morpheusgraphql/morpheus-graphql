import { exit, stdout } from "process";
import { exec } from "../lib/utils/utils";
import { promisify } from "util";
import glob from "glob";

const root = "examples/code-gen/src";

export const codeGen = async () => {
  try {
    exec("stack install --fast --test morpheus-graphql-code-gen");

    const files = (await promisify(glob)(`${root}/**/*.gql`)).join(" ");

    exec(`morpheus build ${files} --root=${root}`);

    exec(
      "ts-node scripts/local.ts format --fix=true --path=examples/code-gen/**/*hs"
    );

    const changes = exec("git status -s")
      .trim()
      .split(" ")
      .filter((x) => x.includes(".hs"));

    if (changes.length > 0) {
      throw Error(`generated files are corrupted. \n${changes}
      `);
    }
    stdout.write("OK");
  } catch (e) {
    stdout.write(e.message);
    exit(1);
  }
};
