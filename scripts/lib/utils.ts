import { execSync } from "child_process";
import axios from "axios";

const { GH_TOKEN } = process.env;

export const exec = (command: string) =>
  execSync(command, {
    maxBuffer: 10 * 1024 * 1024, // 10MB
    encoding: "utf-8",
  })?.trimEnd();

export const getPRNumber = (msg: string) => {
  const num = / \(#(?<prNumber>[0-9]+)\)$/m.exec(msg)?.groups?.prNumber;
  return num ? parseInt(num, 10) : undefined;
};

export const batchMap = async <T>(
  f: (_: unknown[]) => Promise<T[]>,
  commits: unknown[]
) => {
  // Split commits into batches of 50 to prevent timeouts
  const commitInfoPromises: Promise<T[]>[] = [];

  for (let i = 0; i < commits.length; i += 50) {
    const batch = commits.slice(i, i + 50);
    commitInfoPromises.push(f(batch));
  }

  return (await Promise.all(commitInfoPromises)).flat();
};

export const ghApi = (query: string) =>
  axios
    .post("https://api.github.com/graphql", JSON.stringify({ query }), {
      headers: {
        authorization: `Bearer ${GH_TOKEN}`,
        "content-type": "application/json",
      },
    })
    .then(({ data }) => data.data)
    .catch((err) => Promise.reject(err.message));
