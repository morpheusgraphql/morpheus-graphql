import { pluck, propEq, uniq } from "ramda";
import { ghApiGQL, GH_ORG, GH_REPO } from "../gq";
import { Maybe } from "../types";
import { batchMap, getPRNumber } from "../utils";
import { parseLabel, PR_TYPE, SCOPE } from "./pull-request-types";
import { commitsAfter } from "../git";

type Commit = {
  oid: string;
  message: string;
  associatedPullRequests: {
    nodes: Array<{ number: number; repository: { nameWithOwner: string } }>;
  };
};

type PR = {
  number: number;
  title: string;
  author: { login: string; url: string };
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
    Omit<PR, "labels"> & {
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
      ({ labels, ...rest }): PR => ({
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

const getPRs = (version: string): Promise<PR[]> =>
  batchMap(batchCommitInfo, commitsAfter(version)).then((ghCommits) =>
    batchMap(batchPRInfo, uniq(ghCommits.map(getAssociatedPR).filter(Boolean)))
  );

const fetchChanges = (version: string) =>
  getPRs(version).then((prs) =>
    prs.map(
      ({ labels, ...pr }): Change => ({
        ...pr,
        type: labels.map(parseLabel("pr")).find(Boolean) ?? "chore",
        scopes: labels.map(parseLabel("scope")).filter(Boolean) as SCOPE[],
      })
    )
  );

type Change = Omit<PR, "labels"> & {
  type: PR_TYPE;
  scopes: SCOPE[];
};

const isBreaking = (changes: Change[]) =>
  Boolean(changes.find(propEq("type", "breaking")));

export { fetchChanges, isBreaking, Change };
