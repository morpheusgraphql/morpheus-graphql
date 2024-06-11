import { writeFile } from "fs/promises";
import { GHRelEasy, runCli } from "gh-rel-easy";
import { Command } from "commander";

export const exit = (error: Error) => {
  console.log(error.message);
  process.exit(1);
};

// HConf
const hconf = runCli("hconf");

// GHRelEasy
const release = new GHRelEasy({
  gh: "morpheusgraphql/morpheus-graphql",
  scope: {
    server: "morpheus-graphql",
    client: "morpheus-graphql-client",
    core: "morpheus-graphql-core",
    subscriptions: "morpheus-graphql-subscriptions",
    tests: "morpheus-graphql-tests",
    app: "morpheus-graphql-app",
  },
  version: () => hconf("version"),
  pkg: (p) => `https://hackage.haskell.org/package/${p}`,
  next: (b) => hconf("next", ...(b ? ["-b"] : [])).then(console.log),
});

// CLI
const cli = new Command();

cli.name("release-cli").description("Automated Releases").version("1.0");

cli
  .command("open")
  .option("-p, --preview", "preview", false)
  .action(({ preview }: { preview: boolean }) =>
    release
      .changelog()
      .then((body) =>
        hconf("setup")
          .then(console.log)
          .then(() => (preview ? undefined : release.open(body)))
      )
      .catch(exit)
  );

cli.command("changelog").action(() =>
  release
    .changelog()
    .then((body: string) => writeFile("./changelog.md", body, "utf8"))
    .catch(exit)
);

cli.parse();
