import { GHRelEasy } from "gh-rel-easy";

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
  version: "hconf version",
  next: "hconf next",
  setup: "hconf setup",
});

gh.cli();
