import { GHRelEasy, CLI } from "gh-rel-easy";
import { Command } from "commander";

const hconf = new CLI("hconf");

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
  version: () => hconf.exec("version"),
  next: (b) => hconf.void("next", b && "-b"),
  setup: () => hconf.void("setup"),
});

const cli = new Command()
  .name("release-cli")
  .description("Automated Releases")
  .version("1.0");

cli
  .command("open")
  .option("-d, --dry", "only changelog and setup", false)
  .action(({ dry }: { dry: boolean }) => gh.release(dry));

cli
  .command("changelog")
  .action(() => gh.changelog("changelog").then(() => undefined));

cli.parse();
