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

type PR = {
  number: number;
  title: string;
  body: string;
  author: { login: string; url: string };
  labels: { nodes: { name: string }[] };
};

type Change = PR & {
  type: PR_TYPE;
  scopes: SCOPE[];
};

const toPRNumber = (c: Commit): Maybe<number> =>
  c.associatedPullRequests.nodes.find(({ repository }) => isOwner(repository))
    ?.number ?? getPRNumber(c.message);

const fetchChanges = (version: string) =>
  batch<string, Commit>(
    (i) =>
      `object(oid: "${i}") {
          ... on Commit {
              message
              associatedPullRequests(first: 10) { nodes {
                number
                repository { nameWithOwner }
              }}
          }
      }`,
    commitsAfter(version)
  )
    .then((commit) =>
      batch<number, PR>(
        (i) => `pullRequest(number: ${i}) {
                  number
                  title
                  body
                  author { login url }
                  labels(first: 10) { nodes { name } }
                }`,
        uniq(reject(isNil, commit.map(toPRNumber)))
      )
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
