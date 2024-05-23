import axios from "axios";
import { push } from "./git";

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

const gqlAPI = (query: string) => gh("graphql", { query });

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

const gql =
  <T, O>(f: (_: T) => string) =>
  (xs: T[]): Promise<O[]> =>
    gqlAPI(`
        {
            repository(owner: "${GH_ORG}", name: "${GH_REPO}") {
                  ${xs.map(f).join("\n")}
            }
        }
    `).then(
      ({ repository }) => Object.values(repository).filter(Boolean) as O[]
    );

const isOwner = (name: string) => name === `${GH_ORG}/${GH_REPO}`;

export { openPR, isOwner, gql, authUrl };
