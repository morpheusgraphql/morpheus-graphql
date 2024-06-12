import { GHRelEasy, runCli } from "gh-rel-easy";
import { Command } from "commander";

// HConf
const hconf = runCli("hconf");

// GHRelEasy
const gh = new GHRelEasy({
  gh: "morpheusgraphql/morpheus-graphql",
  scope: {
    server: "morpheus-graphql",
    client: "morpheus-graphql-client",
    core: "morpheus-graphql-core",
    subscriptions: "morpheus-graphql-subscriptions",
    tests: "morpheus-graphql-tests",
    app: "morpheus-graphql-app",
  },
  pkg: (p) => `https://hackage.haskell.org/package/${p}`,
  version: () => hconf("version"),
  next: (b) => hconf("next", b && "-b").then(console.log),
  setup: () => hconf("setup").then(console.log),
});

// CLI
const cli = new Command();

cli.name("release-cli").description("Automated Releases").version("1.0");

cli
  .command("open")
  .option("-d, --dry", "only changelog and setup", false)
  .action(({ dry }: { dry: boolean }) => gh.release(dry));

cli.command("changelog").action(async () => {
  await gh.changelog("changelog");
});

cli.parse();
