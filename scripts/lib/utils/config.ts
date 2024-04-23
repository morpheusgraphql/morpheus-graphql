import { readYAML, write } from "./file";
import { Dict, PkgName, Version } from "./types";
import { map } from "ramda";
import {
  VersionUpdate,
  compareVersion,
  genVersion,
  parseVersion,
} from "./version";
import { dump } from "js-yaml";
import { Bounds, Rules, formatRule, parseBound, parseRule } from "./rule";

const STACK_CONFIG_URL = "./config/stack.yaml";

export type StackPlan = {
  deps?: Dict<PkgName>;
  resolver: string;
  include?: PkgName[];
  skip?: PkgName[];
};

export type Config<R extends boolean = false> = {
  version: Version;
  bounds: Bounds<R>;
  rules: Rules<R>;
  plan: Dict<StackPlan>;
  packages: PkgName[];
  examples: PkgName[];
};

const compareConfigKeys = (a: string, b: string) => {
  try {
    return compareVersion(a, b);
  } catch {
    const x = a.toLowerCase();
    const y = b.toLowerCase();
    if (x < y) {
      return -1;
    }
    if (x > y) {
      return 1;
    }
    return 0;
  }
};

export const writeConfig = ({ rules, bounds, ...config }: Config) => {
  write(
    STACK_CONFIG_URL,
    dump(
      {
        ...config,
        bounds: formatRule(bounds),
        rules: map<Rules, Rules<true>>(formatRule, rules),
      },
      {
        sortKeys: compareConfigKeys,
        lineWidth: 240,
        condenseFlow: true,
      }
    )
  );
};

export const getConfig = async (): Promise<Config> => {
  const { rules, bounds, ...rest } = await readYAML<Config<true>>(
    STACK_CONFIG_URL
  );

  return {
    ...rest,
    bounds: parseBound(bounds),
    rules: map<Rules<true>, Rules>(parseRule, rules),
  };
};

export const updateConfig = async ({
  next,
  prev,
  isBreaking,
}: VersionUpdate): Promise<Config> => {
  const { version, bounds, ...rest } = await getConfig();

  if (prev !== version) {
    throw new Error(`invalid versions ${version} and ${prev}`);
  }

  const upper = genVersion(parseVersion(next), true).join(".");
  const newBounds: Bounds = isBreaking ? [next, upper] : bounds;

  return {
    ...rest,
    bounds: newBounds,
    version: next,
  };
};
