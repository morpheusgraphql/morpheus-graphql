import { exit } from "process";
import { exec, log } from "../lib/utils/utils";
// import { promisify } from "util";
// import glob from "glob";

type Options = {
  src?: string;
};

const root = "examples/code-gen";

const noChanges = (label: string) => {
  const changes = exec("git status -s")
    .trim()
    .split("\n")
    .flatMap((file) => file.split(" "))
    .map((x) => x.trim())
    .filter((x) => x.includes(".hs"));

  if (changes.length > 0) {
    throw Error([label, ...changes].join("\n  "));
  }
};

export const codeGen = async ({ src = root }: Options = {}) => {
  try {
    log("installing code-gen\n");

    exec("stack install --fast --test morpheus-graphql-code-gen", "pipe");

    exec(`morpheus build ${src}`, "inherit");

    const hsPath = `${src}/**/*hs`;

    noChanges("generated files are corrupted!");

    exec(`ts-node scripts/local.ts format --fix=true --path=${hsPath}`);

    noChanges("generated files are not formatted properly!");

    log("OK\n", "success");
  } catch (e) {
    log(e.message + "\n", "error");
    exit(1);
  }
};
