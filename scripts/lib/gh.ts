import axios from "axios";
import { push } from "./git";
import { chunks } from "./utils";

const GH_ORG = "morpheusgraphql";

const GH_REPO = "morpheus-graphql";

const token = () => {
  const { GITHUB_TOKEN } = process.env;

  if (!GITHUB_TOKEN) {
    throw new Error("missing variable: GITHUB_TOKEN");
  }
  return GITHUB_TOKEN;
};

const authUrl = () => `https://${token()}@github.com/${GH_ORG}/${GH_REPO}.git`;

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
  return gh(`repos/${GH_ORG}/${GH_REPO}/pulls`, {
    head: branchName,
    draft: true,
    base: "main",
    owner: GH_ORG,
    repo: GH_REPO,
    title,
    body,
  });
};

const batch = <I, O>(f: (_: I) => string, items: I[]) =>
  Promise.all(
    chunks(items).map((batch) =>
      gh("graphql", {
        query: `{
          repository(owner: "${GH_ORG}", name: "${GH_REPO}") {
          ${batch.map((n) => `item_${n}:${f(n)}`).join("\n")}
        }
      }`,
      }).then(({ repository }) => Object.values(repository))
    )
  ).then((x) => x.flat().filter(Boolean) as O[]);

const isOwner = ({ nameWithOwner }: { nameWithOwner: string }) =>
  nameWithOwner === `${GH_ORG}/${GH_REPO}`;

export { openPR, isOwner, batch, authUrl };
