import axios from "axios";

const GH_ORG = "morpheusgraphql";

const GH_REPO = "morpheus-graphql";

const { GITHUB_TOKEN } = process.env;

if (!GITHUB_TOKEN) {
  throw new Error("missing variable: GITHUB_TOKEN");
}

const authorizedGithubUrl = () =>
  `https://${GITHUB_TOKEN}@github.com/${GH_ORG}/${GH_REPO}.git`;

const ghApiREST = (path: string, body: {}) =>
  axios
    .post(`https://api.github.com/${path}`, JSON.stringify(body), {
      headers: {
        authorization: `Bearer ${GITHUB_TOKEN}`,
        "content-type": "application/json",
        accept: "Accept: application/vnd.github.v3+json",
      },
    })
    .then(({ data }) => data.data)
    .catch((err) => Promise.reject(err.message));

const ghApiGQL = (query: string) => ghApiREST("graphql", { query });

export { GH_ORG, GH_REPO, ghApiREST, ghApiGQL, authorizedGithubUrl };
