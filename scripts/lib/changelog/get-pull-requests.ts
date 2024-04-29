import { pluck, uniq } from "ramda";
import { ghApiGQL, GH_ORG, GH_REPO } from "../utils/gq-api";
import { Maybe } from "../utils/types";
import { batchMap, getPRNumber } from "../utils/utils";
import { parseLabel, PR_TYPE, SCOPE } from "./pull-request-types";

type AssocRP = {
  number: number;
  repository: { nameWithOwner: string };
};

type Commit = {
  oid: string;
  message: string;
  associatedPullRequests: { nodes: AssocRP[] };
};

type Author = { login: string; url: string };

type GithubPR = {
  number: number;
  title: string;
  author: Author;
  labels: string[];
  body: string;
};

const getGithub =
  <O>(f: (_: unknown) => string) =>
  async (xs: unknown[]): Promise<O[]> => {
    const { repository } = await ghApiGQL(`
        {
            repository(owner: "${GH_ORG}", name: "${GH_REPO}") {
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

const batchPRInfo = (xs: unknown[]) =>
  getGithub<
    Omit<GithubPR, "labels"> & {
      labels: { nodes: { name: string }[] };
    }
  >(
    (number) => `
    pr_${number}: pullRequest(number: ${number}) {
        number
        title
        url
        body
        author {
          login
          url
        }
        labels(first: 10) {
            nodes {
              name
            }
        }
    }
    `
  )(xs).then((prs) =>
    prs.map(
      ({ labels, ...rest }): GithubPR => ({
        ...rest,
        labels: pluck("name", labels.nodes),
      })
    )
  );

const getAssociatedPR = ({
  associatedPullRequests,
  message,
}: Commit): Maybe<number> => {
  const number = associatedPullRequests.nodes.find(
    ({ repository: { nameWithOwner } }) =>
      nameWithOwner === `${GH_ORG}/${GH_REPO}`
  )?.number;

  return number ?? getPRNumber(message);
};

const getGithubPRs = (commits: string[]): Promise<GithubPR[]> =>
  batchMap(batchCommitInfo, commits).then((ghCommits) =>
    batchMap(batchPRInfo, uniq(ghCommits.map(getAssociatedPR).filter(Boolean)))
  );

const getPullRequests = (commits: string[]) =>
  getGithubPRs(commits).then((prs) =>
    prs.map(
      ({ labels, ...pr }): PullRequest => ({
        ...pr,
        type: labels.map(parseLabel("pr")).find(Boolean) ?? "chore",
        scopes: labels.map(parseLabel("scope")).filter(Boolean) as SCOPE[],
      })
    )
  );

type PullRequest = Omit<GithubPR, "labels"> & {
  type: PR_TYPE;
  scopes: SCOPE[];
};

export { getPullRequests, PullRequest };
