import { isNil, map, pluck, propEq, reject, uniq } from "ramda";
import { github } from "../gh";
import { Maybe } from "../types";
import { getPRNumber } from "../utils";
import { Config, parseLabels, PRType } from "./types";
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
  type: PRType;
  scopes: string[];
};

const toPRNumbers = (commit: Commit[]) =>
  uniq(reject(isNil, commit.map(toPRNumber)));

const fetchChanges = (config: Config, version: string) =>
  fetchCommits(commitsAfter(version))
    .then(toPRNumbers)
    .then(fetchPPs)
    .then(
      map((pr): Change => {
        const labels = pluck("name", pr.labels.nodes);

        return {
          ...pr,
          type: parseLabels(config, "pr", labels).find(Boolean) ?? "chore",
          scopes: parseLabels(config, "scope", labels),
        };
      })
    );

const isBreaking = (changes: Change[]) =>
  Boolean(changes.find(propEq("type", "breaking")));

export { fetchChanges, isBreaking, Change };
