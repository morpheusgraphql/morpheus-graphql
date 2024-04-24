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
import {
  Bounds,
  Rules,
  formatRule,
  parseBound,
  parseRule,
  withRule,
} from "./rule";
import path from "path";

const STACK_CONFIG_URL = "./config/stack.yaml";

export type StackPlan = {
  deps?: Dict<PkgName>;
  resolver: string;
  include?: PkgName[];
  skip?: PkgName[];
};

type _Config<R extends boolean = false> = {
  version: Version;
  bounds: Bounds<R>;
  rules: Rules<R>;
  plan: Dict<StackPlan>;
  libs: PkgName[];
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

const writeConfig = ({ rules, bounds, ...config }: _Config) => {
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

const getConfig = async (): Promise<_Config> => {
  const { rules, bounds, ...rest } = await readYAML<_Config<true>>(
    STACK_CONFIG_URL
  );

  return {
    ...rest,
    bounds: parseBound(bounds),
    rules: map<Rules<true>, Rules>(parseRule, rules),
  };
};

const updateConfig = async ({
  next,
  prev,
  isBreaking,
}: VersionUpdate): Promise<_Config> => {
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

const PREFIX = "morpheus-graphql";

const required = <T>(p: T, message: string) => {
  if (!p) {
    throw new Error(message);
  }

  return p;
};

export class Config {
  constructor(private config: _Config) {}

  static read = async (change?: VersionUpdate) =>
    new Config(await (change ? updateConfig(change) : getConfig()));

  packages = () => [
    ...this.config.libs.map((name) =>
      name === "main" ? PREFIX : `${PREFIX}-${name}`
    ),
    ...this.config.examples.map((name) => path.join("examples", name)),
  ];

  get version() {
    return this.config.version;
  }

  checkDependency(name: string, hasNoBounds: boolean): string[] {
    if (name.startsWith(PREFIX)) {
      if (hasNoBounds) {
        return [name];
      }
      return withRule(name, this.config.bounds);
    }

    const rule = this.rule(name);

    if (rule) {
      return typeof rule === "boolean" ? [name] : withRule(name, rule);
    }

    throw new Error(`Unknown package: ${name}`);
  }

  plan = (version: string) =>
    required(
      this.config.plan[version],
      `ghc version ${version} is not supported! supported versions are: \n - ${this.plans().join(
        "\n - "
      )}`
    );

  rule = (name: string) => this.config.rules[name];

  plans = () =>
    Object.keys(this.config.plan).sort((a, b) => compareVersion(b, a));

  write = () => writeConfig(this.config);

  update = updateConfig;
}
