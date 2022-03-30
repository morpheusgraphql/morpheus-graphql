import axios from "axios";

const { GH_TOKEN } = process.env;

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

export const isKey = <T extends string>(
  obj: Record<T, unknown>,
  key?: string | null
): key is T => typeof key === "string" && key in obj;

export const exit = (error: unknown) => {
  console.error(error);
  process.exit(1);
};
