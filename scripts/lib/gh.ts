import axios from "axios";
import { push } from "./git";
import { chunks } from "./utils";

const ORG = "morpheusgraphql";

const REPO = "morpheus-graphql";

const token = () => {
  const { GITHUB_TOKEN } = process.env;

  if (!GITHUB_TOKEN) {
    throw new Error("missing variable: GITHUB_TOKEN");
  }
  return GITHUB_TOKEN;
};

const GH_PATH = `github.com/${ORG}/${REPO}`;

const authUrl = () => `https://${token()}@${GH_PATH}.git`;

const issueURL = (n: number) => `https://${GH_PATH}/issues/${n}`;

const gh = (path: string, body: {}) =>
  axios
    .post(`https://api.github.com/${path}`, JSON.stringify(body), {
      headers: {
        authorization: `Bearer ${token()}`,
        "content-type": "application/json",
        accept: "Accept: application/vnd.github.v3+json",
      },
    })
    .then(({ data }) => data.data)
    .catch((err) => Promise.reject(err.message));

const openPR = (branchName: string, title: string, body: string) => {
  push(branchName);
  return gh(`repos/${ORG}/${REPO}/pulls`, {
    head: branchName,
    draft: true,
    base: "main",
    owner: ORG,
    repo: REPO,
    title,
    body,
  });
};

const batch =
  <I, O>(f: (_: I) => string) =>
  (items: I[]) =>
    Promise.all(
      chunks(items).map((chunk) =>
        gh("graphql", {
          query: `{
          repository(owner: "${ORG}", name: "${REPO}") {
          ${chunk.map((n) => `item_${n}:${f(n)}`).join("\n")}
        }
      }`,
        }).then(({ repository }) => Object.values(repository))
      )
    ).then((x) => x.flat().filter(Boolean) as O[]);

const isOwner = ({ nameWithOwner }: { nameWithOwner: string }) =>
  nameWithOwner === `${ORG}/${REPO}`;

export { openPR, isOwner, batch, authUrl, issueURL };
