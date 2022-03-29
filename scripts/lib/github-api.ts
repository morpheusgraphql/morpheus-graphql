import { uniq } from "ramda";
import { batchMap, getPRNumber, ghApi } from "./utils";

const githubOrg = "morpheusgraphql";
const githubRepo = "morpheus-graphql";

type AsoccRP = {
  number: number;
  repository: { nameWithOwner: string };
};

type Commit = {
  oid: string;
  message: string;
  associatedPullRequests: { nodes: AsoccRP[] };
};

type Author = {
  login: string;
  url: string;
  name: string;
};

export type GithubPR = {
  number: number;
  title: string;
  url: string;
  author: Author;
  labels: { nodes: [] };
  body: string;
};

const getGithub =
  <O>(f: (_: unknown) => string) =>
  async (xs: unknown[]): Promise<O[]> => {
    const { repository } = await ghApi(`
        {
            repository(owner: "${githubOrg}", name: "${githubRepo}") {
                  ${xs.map(f).join("\n")}
            }
        }
    `);

    return Object.values(repository).filter(Boolean) as any;
  };

const batchCommitInfo = getGithub<Commit>(
  (oid) =>
    `
    commit_${oid}: object(oid: "${oid}") {
        ... on Commit {
            oid
            message
            associatedPullRequests(first: 10) {
            nodes {
                number
                repository {
                  nameWithOwner
                }
              }
            }
        }
    }
    `
);

const batchPRInfo = getGithub<GithubPR>(
  (number) => `
    pr_${number}: pullRequest(number: ${number}) {
        number
        title
        url
        body
        author {
            login
            url
            ... on User {
            name
            }
        }
        labels(first: 10) {
            nodes {
            name
            }
        }
    }
    `
);

const getAsoccPR = ({
  associatedPullRequests,
  oid,
  message,
}: Commit): number => {
  const pr = associatedPullRequests.nodes.find(
    ({ repository: { nameWithOwner } }) =>
      nameWithOwner === `${githubOrg}/${githubRepo}`
  );

  const num = pr?.number ?? getPRNumber(message);

  if (!num) {
    throw new Error(`Commit ${oid} has no associated PR: ${message}`);
  }

  return num;
};

export const getPRsInfo = (commits: string[]): Promise<GithubPR[]> =>
  batchMap(batchCommitInfo, commits).then((ghCommits) =>
    batchMap(batchPRInfo, uniq(ghCommits.map(getAsoccPR)))
  );
