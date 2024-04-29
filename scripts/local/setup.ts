import { difference } from "ramda";
import { checkPackages } from "../lib/check-packages";
import { writeYAML } from "../lib/utils/file";
import { Config } from "../lib/utils/config";
import { log } from "../lib/utils/utils";
import { Version } from "../lib/utils/version";
import { hie } from "./hie";

const getStack = async (version: string) => {
  const config = await Config.read();
  const plans = config.plans();
  const { include = [], resolver, skip = [] } = config.plan(version);
  const versions = plans.filter((v) => Version.compare(v, version) >= 0);

  const extraDeps: Record<string, string> = Object.fromEntries(
    versions.flatMap((v) => Object.entries(config.plan(v).deps ?? {}))
  );

  return {
    ...(version === "latest" ? { "allow-newer": true } : {}),
    resolver,
    "save-hackage-creds": false,
    packages: difference([...config.packages(), ...include], skip),
    "extra-deps": Object.entries(extraDeps)
      .map(([key, val]) => `${key}-${val}`)
      .sort(),
  };
};

const ok = (msg: string) => log(` - ${msg}\n`, "success");

export const setup = async (version: string) => {
  const config = await Config.read();

  log("generating:\n");

  writeYAML("stack.yaml", await getStack(version));

  ok(`stack.yaml (ghc ${version})`);

  hie([...config.packages(), "morpheus-graphql-benchmarks"]);

  ok("hie.yaml");

  checkPackages();
};
