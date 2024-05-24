import { isNil, map, pluck, propEq, reject, uniq } from "ramda";
import { isOwner, batch } from "../gh";
import { Maybe } from "../types";
import { getPRNumber } from "../utils";
import { parseLabel, PR_TYPE, SCOPE } from "./pull-request-types";
import { commitsAfter } from "../git";

type Commit = {
  message: string;
  associatedPullRequests: {
    nodes: Array<{ number: number; repository: { nameWithOwner: string } }>;
  };
};

const ghCommit = (i: string) =>
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
  }`;

const toPRNumber = (c: Commit): Maybe<number> =>
  c.associatedPullRequests.nodes.find(({ repository }) => isOwner(repository))
    ?.number ?? getPRNumber(c.message);

type PR = {
  number: number;
  title: string;
  body: string;
  author: { login: string; url: string };
  labels: { nodes: { name: string }[] };
};

const ghPR = (i: number) => `pullRequest(number: ${i}) {
  number
  title
  body
  author { login url }
  labels(first: 10) { nodes { name } }
}`;

type Change = PR & {
  type: PR_TYPE;
  scopes: SCOPE[];
};

const fetchChanges = (version: string) =>
  batch<string, Commit>(ghCommit, commitsAfter(version))
    .then((commit) =>
      batch<number, PR>(ghPR, uniq(reject(isNil, commit.map(toPRNumber))))
    )
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
