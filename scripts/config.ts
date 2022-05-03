import { Command } from "commander";
import { difference } from "ramda";
import { getConfig, writeYAML } from "./lib/utils/file";
import { compareVersion } from "./lib/utils/version";

const cli = new Command();

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
    packages: difference([...examples, ...include, ...packages], skip),
    "extra-deps": Object.entries(extraDeps)
      .map(([key, val]) => `${key}-${val}`)
      .sort(),
  };
};

const setup = async (version: string) => {
  const { plan } = await getConfig();

  if (version !== "all") {
    return writeYAML("stack.yaml", await getStack(version));
  }

  writeYAML("stack.yaml", await getStack("latest"));
  Object.keys(plan).forEach(async (v) =>
    writeYAML(`./config/stack/${v}.yaml`, await getStack(v))
  );
};

cli.name("config").description("setup stack config").version("0.0.0");

cli
  .command("setup")
  .description("config stack env")
  .argument("<string>", "version number")
  .action(setup);

cli
  .command("all")
  .description("config stack env")
  .action(async () => {});

cli.parse();
