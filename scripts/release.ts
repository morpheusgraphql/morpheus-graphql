import { GHRelEasy, CLI } from "gh-rel-easy";

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
  pkg: "https://hackage.haskell.org/package/{{SCOPE}}",
  version: () => hconf.exec("version"),
  next: (b) => hconf.void("next", b && "-b"),
  setup: () => hconf.void("setup"),
});

gh.cli();
