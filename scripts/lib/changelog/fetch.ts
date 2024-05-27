import { isNil, map, pluck, propEq, reject, uniq } from "ramda";
import { github } from "../gh";
import { Maybe } from "../types";
import { getPRNumber } from "../utils";
import { parseLabel, PR_TYPE, SCOPE } from "./types";
import { commitsAfter } from "../git";

type Commit = {
  message: string;
  associatedPullRequests: {
    nodes: Array<{ number: number; repository: { nameWithOwner: string } }>;
  };
};

const fetchCommits = github.batch<Commit>(
  (i) =>
    `object(oid: "${i}") {
    ... on Commit {
      message
      associatedPullRequests(first: 10) { 
        nodes {
          number
          repository { nameWithOwner }
        }
      }
    }
  }`
);

const toPRNumber = (c: Commit): Maybe<number> =>
  c.associatedPullRequests.nodes.find(({ repository }) =>
    github.isOwner(repository)
  )?.number ?? getPRNumber(c.message);

type PR = {
  number: number;
  title: string;
  body: string;
  author: { login: string; url: string };
  labels: { nodes: { name: string }[] };
};

const fetchPPs = github.batch<PR>(
  (i) =>
    `pullRequest(number: ${i}) {
      number
      title
      body
      author { login url }
      labels(first: 10) { nodes { name } }
    }`
);

type Change = PR & {
  type: PR_TYPE;
  scopes: SCOPE[];
};

const toPRNumbers = (commit: Commit[]) =>
  uniq(reject(isNil, commit.map(toPRNumber)));

const fetchChanges = (version: string) =>
  fetchCommits(commitsAfter(version))
    .then(toPRNumbers)
    .then(fetchPPs)
    .then(
      map((pr): Change => {
        const labels = pluck("name", pr.labels.nodes);

        return {
          ...pr,
          type: labels.map(parseLabel("pr")).find(Boolean) ?? "chore",
          scopes: labels.map(parseLabel("scope")).filter(Boolean) as SCOPE[],
        };
      })
    );

const isBreaking = (changes: Change[]) =>
  Boolean(changes.find(propEq("type", "breaking")));

export { fetchChanges, isBreaking, Change };
