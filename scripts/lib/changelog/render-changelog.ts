import { groupBy, range } from "ramda";
import { isKey } from "../utils";
import { Change } from "./fetch";
import { pullRequestTypes, config, SCOPE } from "./pull-request-types";
import { getDate } from "../git";

const link = (name: string, url: string) => `[${name}](${url})`;

const issueURL = (n: number) =>
  `https://github.com/morpheusgraphql/morpheus-graphql/issues/${n}`;

const packageURL = (name: SCOPE) =>
  `https://hackage.haskell.org/package/${config.scope[name]}`;

const renderScope = (scope: SCOPE) => link(scope, packageURL(scope));

const indent = (x: number) =>
  range(0, x * 2)
    .map(() => " ")
    .join("");

const renderStats = (topics: [string, string][]) =>
  topics
    .filter(([_, value]) => value)
    .map(([topic, value]) => `${indent(1)}- ${topic} ${value}`)
    .join("\n");

const renderPullRequest = ({
  number,
  author,
  title,
  body,
  scopes,
}: Change): string => {
  const details = body
    ? `${indent(1)}- <details>\n${indent(3)}${body.replace(
        /\n/g,
        `\n${indent(3)}`
      )}\n${indent(1)}  </details>`
    : "";

  const head = `* ${link(`#${number}`, issueURL(number))}: ${title?.trim()}`;

  const stats = renderStats([
    ["📦", scopes.map(renderScope).join(", ")],
    ["👤", link(`@${author.login}`, author.url)],
    ["📎", ""],
  ]);

  return [head, stats, details].filter(Boolean).join("\n");
};

const renderSection = (label: string, pullRequests: Change[]) =>
  [`#### ${label}`, pullRequests.map(renderPullRequest)].flat().join("\n");

const renderChangelog = (tag: string, changes: Change[]) => {
  const groups = groupBy(({ type }) => type, changes);

  return [
    `## ${tag || "Unreleased"} (${getDate()})\n`,
    Object.entries(pullRequestTypes)
      .flatMap(([type, label]) =>
        isKey(groups, type) ? renderSection(label, groups[type]) : ""
      )
      .filter(Boolean)
      .join("\n\n"),
  ].join("\n");
};

export { renderChangelog };
