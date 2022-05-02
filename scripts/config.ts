import { Command } from "commander";
import { difference } from "ramda";
import { getConfig, writeYAML } from "./lib/utils/file";
import { isHigherOrEQ, parseVersion } from "./lib/utils/version";

const cli = new Command();

const setup = async (version: string) => {
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

  writeYAML("stack.yaml", {
    resolver: current.resolver,
    "save-hackage-creds": false,
    packages: difference([...examples, ...include, ...packages], skip),
    "extra-deps": extraDeps,
  });
};

cli.name("config").description("setup stack config").version("0.0.0");

cli
  .command("setup")
  .description("config stack env")
  .argument("<string>", "version number")
  .action((version: string) => {
    console.log(version);
    setup(version);
  });

cli.parse();
