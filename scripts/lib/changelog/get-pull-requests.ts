import { isNil, pluck, propEq, reject, uniq } from "ramda";
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
  labels: { nodes: { name: string }[] };
  body: string;
};

const gh =
  <T, O>(f: (_: T) => string) =>
  async (xs: T[]): Promise<O[]> => {
    const { repository } = await ghApiGQL(`
        {
            repository(owner: "${GH_ORG}", name: "${GH_REPO}") {
                  ${xs.map(f).join("\n")}
            }
        }
    `);

    return Object.values(repository).filter(Boolean) as any;
  };

const batchCommit = gh<string, Commit>(
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
  gh<number, PR>(
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
  )(xs).then((prs) =>
    prs.map((pr): Change => {
      const labels = pluck("name", pr.labels.nodes);

      return {
        ...pr,
        type: labels.map(parseLabel("pr")).find(Boolean) ?? "chore",
        scopes: labels.map(parseLabel("scope")).filter(Boolean) as SCOPE[],
      };
    })
  );

const getPR = ({ associatedPullRequests, message }: Commit): Maybe<number> => {
  const number = associatedPullRequests.nodes.find(
    ({ repository: { nameWithOwner } }) =>
      nameWithOwner === `${GH_ORG}/${GH_REPO}`
  )?.number;

  return number ?? getPRNumber(message);
};

const fetchChanges = (version: string) =>
  batchMap(batchCommit, commitsAfter(version)).then((commit) =>
    batchMap(batchPR, uniq(reject(isNil, commit.map(getPR))))
  );

const isBreaking = (changes: Change[]) =>
  Boolean(changes.find(propEq("type", "breaking")));

export { fetchChanges, isBreaking, Change };
