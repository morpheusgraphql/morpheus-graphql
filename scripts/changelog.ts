import { exec } from "./lib/utils";
import parse from "@commitlint/parse";
import { groupBy } from "ramda";
import { getPRsInfo, GithubPR } from "./lib/github-api";

const labelsConfig = {
  feat: "New Feature",
  fix: "Bug Fix",
  internal: "Internal",
  dependency: "Dependency",
  docs: "Docs",
  brakingChange: "Breaking Change",
  deprecation: "Deprecation",
};

type PRType = keyof typeof labelsConfig;

type PR = GithubPR & {
  type: PRType;
  scope?: string;
  subject?: string;
  number?: number;
};

const isKey = <T extends string>(
  obj: Record<T, unknown>,
  key?: string | null
): key is T => typeof key === "string" && key in obj;

const getChangeLog = async () => {
  const date = exec("git log -1 --format=%cd --date=short");
  const tag = exec("git describe --abbrev=0 --tags");
  const commits: string[] = exec(`git rev-list --reverse ${tag}..`).split("\n");

  const prs = await getPRsInfo(commits);

  const commitInfos = await Promise.all(
    prs.map(async (pr): Promise<PR> => {
      const { type, subject, header, scope } = await parse(pr.title);

      return {
        ...pr,
        type: isKey(labelsConfig, type) ? type : "internal",
        subject: subject ?? header,
        scope: scope ?? undefined,
      };
    })
  );

  return renderChangelog(tag, date, commitInfos);
};

function renderChangelog(tag: string, date: string, prs: PR[]) {
  const groups = groupBy((x) => x.type, prs);
  let changelog = `## ${tag || "Unreleased"} (${date})\n`;

  return [
    changelog,
    Object.entries(labelsConfig)
      .flatMap(([type, label]) =>
        isKey(groups, type) ? renderSection(label, groups[type]) : ""
      )
      .filter(Boolean)
      .join("\n\n"),
  ].join("\n");
}

const renderSection = (label: string, pullRequests: PR[]) =>
  [`#### ${label}`, pullRequests.map(renderPullRequest)].flat().join("\n");

const renderHead = (number: number) => {
  const url = `https://github.com/morpheusgraphql/morpheus-graphql/issues/${number}`;
  return `[#${number}](${url})`;
};

const renderPullRequest = ({ number, author, subject, body }: PR): string => {
  const refs = author ? `([@${author.login}](${author.url}))` : "";
  const details = body
    ? `\n  <details>\n   ${body.replace(/\n/g, "\n    ")}\n  </details>`
    : "";

  return ["*", number ? renderHead(number) : "", subject?.trim(), refs, details]
    .filter(Boolean)
    .join(" ");
};

getChangeLog()
  .then((changelog) => process.stdout.write(changelog))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
