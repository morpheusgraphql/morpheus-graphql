import { isNil, map, pluck, propEq, reject, uniq } from "ramda";
import { gql, isOwner } from "../gq";
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
  labels: { nodes: { name: string }[] };
  body: string;
};

const batchCommit = gql<string, Commit>(
  (oid) =>
    `commit_${oid}: object(oid: "${oid}") {
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
    }`
);

type Change = PR & {
  type: PR_TYPE;
  scopes: SCOPE[];
};

const batchPR = (xs: number[]) =>
  gql<number, PR>(
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
    }`
  )(xs);

const ToPRNumber = ({
  associatedPullRequests,
  message,
}: Commit): Maybe<number> => {
  const number = associatedPullRequests.nodes.find(
    ({ repository: { nameWithOwner } }) => isOwner(nameWithOwner)
  )?.number;

  return number ?? getPRNumber(message);
};

const fetchChanges = (version: string) =>
  batchMap(batchCommit, commitsAfter(version))
    .then((commit) =>
      batchMap(batchPR, uniq(reject(isNil, commit.map(ToPRNumber))))
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
