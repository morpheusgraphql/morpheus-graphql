import { difference } from "ramda";
import { checkPackages } from "../lib/check-packages";
import { StackPlan } from "../lib/check-packages/types";
import { getConfig, writeYAML } from "../lib/utils/file";
import { compareVersion } from "../lib/utils/version";
import { hie } from "./hie";

const getStack = async (version: string) => {
  const { plan, examples, packages } = await getConfig();
  const plans = Object.keys(plan).sort((a, b) => compareVersion(b, a));
  const current = plan[version];

  if (!current) {
    throw new Error(
      `ghc version ${version} is not supported! supported versions are: \n - ${plans.join(
        "\n - "
      )}`
    );
  }

  const higher = (v: string) => compareVersion(v, version) >= 0;

  const matchingVersions = plans.filter(higher);

  const extraDeps: Record<string, string> = Object.fromEntries(
    matchingVersions.flatMap((v) => Object.entries(plan[v].deps ?? {}))
  );

  const { include = [], skip = [] } = current;

  return {
    ...(version === "latest" ? { "allow-newer": true } : {}),
    resolver: current.resolver,
    "save-hackage-creds": false,
    packages: difference(
      [...examples.map((e) => `examples/${e}`), ...include, ...packages],
      skip
    ),
    "extra-deps": Object.entries(extraDeps)
      .map(([key, val]) => `${key}-${val}`)
      .sort(),
  };
};

const checkStackConfig = async (
  plan: Record<string, StackPlan>,
  version: string
) => {
  const plans = Object.keys(plan);

  await Promise.all(
    plans.map((v) =>
      getStack(v).then((con) => writeYAML(`./config/stack/${v}.yaml`, con))
    )
  );

  writeYAML("stack.yaml", await getStack(version));
};

export const setup = async (version: string) => {
  const { plan, packages, examples } = await getConfig();
  checkStackConfig(plan, version);
  hie([
    ...packages,
    ...examples.map((key) => `examples/${key}`),
    "morpheus-graphql-benchmarks",
  ]);
  checkPackages();
};
