import { Command } from "commander";
import { difference } from "ramda";
import { getConfig, writeYAML } from "./lib/utils/file";
import { isHigherOrEQ, parseVersion } from "./lib/utils/version";

const cli = new Command();

const getStack = async (version: string) => {
  const { plan, examples, packages } = await getConfig();
  const plans = Object.keys(plan);
  const current = plan[version];

  if (!current) {
    throw new Error(
      `ghc version ${version} is not supported! supported versions are: \n - ${plans.join(
        "\n - "
      )}`
    );
  }

  const higher = (v: string) =>
    isHigherOrEQ(parseVersion(v), parseVersion(version));

  const matchingVersions =
    version === "latest"
      ? ["latest"]
      : plans.filter((x) => x === "latest" || higher(x));

  const extraDeps = matchingVersions.flatMap((v) => plan[v].deps ?? []);

  const { include = [], skip = [] } = current;

  return {
    resolver: current.resolver,
    "save-hackage-creds": false,
    packages: difference([...examples, ...include, ...packages], skip),
    "extra-deps": extraDeps,
  };
};

const setup = async (version: string) =>
  writeYAML("stack.yaml", await getStack(version));

cli.name("config").description("setup stack config").version("0.0.0");

cli
  .command("setup")
  .description("config stack env")
  .argument("<string>", "version number")
  .action((version: string) => {
    console.log(version);
    setup(version);
  });

cli
  .command("all")
  .description("config stack env")
  .action(async () => {
    const { plan } = await getConfig();
    Object.keys(plan).forEach(async (v) =>
      writeYAML(`./config/stack/${v}.yaml`, await getStack(v))
    );
  });

cli.parse();
