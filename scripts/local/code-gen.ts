import { exit } from "process";
import { exec, log } from "../lib/utils/utils";
import { promisify } from "util";
import glob from "glob";

type Options = {
  root?: string;
};

export const codeGen = async ({
  root = "examples/code-gen/src",
}: Options = {}) => {
  try {
    log("installing code-gen\n");

    exec("stack install --fast --test morpheus-graphql-code-gen", "pipe");

    const gqlPath = `${root}/**/*.gql`;
    const hsPath = `${root}/**/*hs`;

    const files = await promisify(glob)(gqlPath);

    log(`generating code(${files.length} files): ${gqlPath} \n`);

    exec(`morpheus build ${files.join(" ")} --root=${root}`);

    log(`formatting(${files.length} files): ${hsPath} \n\n`);

    exec(`ts-node scripts/local.ts format --fix=true --path=${hsPath}`);

    const changes = exec("git status -s")
      .trim()
      .split("\n")
      .map((file) => file.trim())
      .filter((x) => x.includes(".hs"));

    if (changes.length > 0) {
      throw Error(["generated files are corrupted!", ...changes].join("\n  "));
    }

    log("OK\n", "success");
  } catch (e) {
    log(e.message + "\n", "error");
    exit(1);
  }
};
