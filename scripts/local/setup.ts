import { difference } from "ramda";
import { checkPackages } from "../lib/check-packages";
import { Yaml } from "../lib/utils/file";
import { Config } from "../lib/utils/config";
import { log } from "../lib/utils/utils";
import { Version } from "../lib/utils/version";
import { hie } from "./hie";
import { defs } from "../lib/utils/defs";

const Stack = new Yaml<unknown, []>(() => defs.STACK);

const getStack = async (version: string) => {
  const config = await Config.read();
  const { include = [], resolver, skip = [] } = config.plan(version);
  const extra = config
    .plans()
    .filter((v) => Version.compare(v, version) >= 0)
    .flatMap((v) => Object.entries(config.plan(v).deps ?? {}))
    .map(([key, val]) => `${key}-${val}`)
    .sort();
  const packages = difference([...config.packages(), ...include], skip);

  return {
    ...(version === "latest" ? { "allow-newer": true } : {}),
    resolver,
    "save-hackage-creds": false,
    packages,
    "extra-deps": extra,
  };
};

const ok = (msg: string) => log(` - ${msg}\n`, "success");

export const setup = async (version: string) => {
  const config = await Config.read();

  log("generating:\n");

  Stack.write(await getStack(version));

  ok(`${defs.STACK} (ghc ${version})`);

  hie([...config.packages(), "morpheus-graphql-benchmarks"]);

  ok(defs.HIE);

  checkPackages();
};
