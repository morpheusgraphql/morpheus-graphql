import { groupBy, range } from "ramda";
import { isKey } from "../utils";
import { Change } from "./fetch";
import { Config } from "./types";
import { getDate } from "../git";
import { github } from "../gh";

const link = (name: string, url: string) => `[${name}](${url})`;

const packageURL = (config: Config, name: string) =>
  `https://hackage.haskell.org/package/${config.scope[name]}`;

const renderScope = (config: Config) => (scope: string) =>
  link(scope, packageURL(config, scope));

const indent = (x: number) =>
  range(0, x * 2)
    .map(() => " ")
    .join("");

const renderStats = (topics: [string, string][]) =>
  topics
    .filter(([_, value]) => value)
    .map(([topic, value]) => `${indent(1)}- ${topic} ${value}`)
    .join("\n");

const renderPullRequest =
  (config: Config) =>
  ({ number, author, title, body, scopes }: Change): string => {
    const details = body
      ? `${indent(1)}- <details>\n${indent(3)}${body.replace(
          /\n/g,
          `\n${indent(3)}`
        )}\n${indent(1)}  </details>`
      : "";

    const head = `* ${link(
      `#${number}`,
      github.issue(number)
    )}: ${title?.trim()}`;

    const stats = renderStats([
      ["📦", scopes.map(renderScope(config)).join(", ")],
      ["👤", link(`@${author.login}`, author.url)],
      ["📎", ""],
    ]);

    return [head, stats, details].filter(Boolean).join("\n");
  };

const renderSection = (config: Config, label: string, pullRequests: Change[]) =>
  [`#### ${label}`, pullRequests.map(renderPullRequest(config))]
    .flat()
    .join("\n");

const render = (config: Config, tag: string, changes: Change[]) => {
  const groups = groupBy(({ type }) => type, changes);

  return [
    `## ${tag || "Unreleased"} (${getDate()})\n`,
    Object.entries(config.pr)
      .flatMap(([type, label]) =>
        isKey(groups, type) ? renderSection(config, label, groups[type]) : ""
      )
      .filter(Boolean)
      .join("\n\n"),
  ].join("\n");
};

export { render };
