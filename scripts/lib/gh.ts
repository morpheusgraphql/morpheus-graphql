import axios from "axios";
import { git } from "./git";
import { chunks, hconf } from "./utils";

const token = () => {
  const { GITHUB_TOKEN } = process.env;

  if (!GITHUB_TOKEN) {
    throw new Error("missing variable: GITHUB_TOKEN");
  }
  return GITHUB_TOKEN;
};

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

class Github {
  constructor(private org: string, private repo: string) {}

  private get path() {
    return `github.com/${this.org}/${this.repo}`;
  }

  public isOwner = ({ nameWithOwner }: { nameWithOwner: string }) =>
    nameWithOwner === `${this.org}/${this.repo}`;

  public batch =
    <O>(f: (_: string | number) => string) =>
    (items: Array<string | number>) =>
      Promise.all(
        chunks(items).map((chunk) =>
          gh("graphql", {
            query: `{
          repository(owner: "${this.org}", name: "${this.repo}") {
          ${chunk.map((n) => `item_${n}:${f(n)}`).join("\n")}
        }
      }`,
          }).then(({ repository }) => Object.values(repository))
        )
      ).then((x) => x.flat().filter(Boolean) as O[]);

  public issue = (n: number) => `https://${this.path}/issues/${n}`;

  public release = async (body: string) => {
    const version = await hconf("version");
    const name = `publish-release/${version}`;

    git("add", ".");
    git("status");
    git("commit", "-m", `"${name}"`);
    git("push", `https://${token()}@${this.path}.git`, `HEAD:${name}`);

    return gh(`repos/${this.org}/${this.repo}/pulls`, {
      head: name,
      draft: true,
      base: "main",
      owner: this.org,
      repo: this.repo,
      title: `Publish Release ${version}`,
      body,
    });
  };
}

export const github = new Github("morpheusgraphql", "morpheus-graphql");
