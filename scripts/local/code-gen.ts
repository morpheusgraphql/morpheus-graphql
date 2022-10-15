import { exit } from "process";
import { exec, log } from "../lib/utils/utils";
// import { promisify } from "util";
// import glob from "glob";

type Options = {
  src?: string;
};

const root = "examples/code-gen";

export const codeGen = async ({ src = root }: Options = {}) => {
  try {
    log("installing code-gen\n");

    exec("stack install --fast --test morpheus-graphql-code-gen", "pipe");

    exec(`morpheus build ${src} -r=""`, "inherit");

    const hsPath = `${src}/**/*hs`;

    exec(`ts-node scripts/local.ts format --fix=true --path=${hsPath}`);

    const changes = exec("git status -s")
      .trim()
      .split("\n")
      .flatMap((file) => file.split(" "))
      .map((x) => x.trim())
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
