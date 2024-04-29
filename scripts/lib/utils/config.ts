import { Yaml } from "./file";
import { Dict, PkgName } from "./types";
import { map } from "ramda";
import { VersionUpdate, Version, StrVersion } from "./version";
import {
  Bounds,
  Rules,
  formatBounds,
  formatRule,
  parseBound,
  parseRule,
  withRule,
} from "./rule";
import path from "path";

const PATH = "./config/stack.yaml";

export type StackPlan = {
  deps?: Dict<PkgName>;
  resolver: string;
  include?: PkgName[];
  skip?: PkgName[];
};

type _Config<R extends boolean = false> = {
  name: string;
  version: StrVersion;
  bounds: Bounds<R>;
  rules: Rules<R>;
  plan: Dict<StackPlan>;
  libs: PkgName[];
  examples: PkgName[];
};

const compareConfigKeys = (a: string, b: string) => {
  try {
    return Version.compare(a, b);
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

const required = <T>(p: T, message: string) => {
  if (!p) {
    throw new Error(message);
  }

  return p;
};

const ConfigYaml = new Yaml<_Config<true>>();

export class Config {
  constructor(private config: _Config) {}

  static read = async (change?: VersionUpdate) => {
    const { rules, bounds, ...rest } = await ConfigYaml.read(PATH);

    const config = new Config({
      ...rest,
      bounds: parseBound(bounds),
      rules: map<Rules<true>, Rules>(parseRule, rules),
    });

    return change ? config.update(change) : config;
  };

  packages = () => [
    ...this.config.libs.map((name) =>
      name === "." ? this.config.name : `${this.config.name}-${name}`
    ),
    ...this.config.examples.map((name) => path.join("examples", name)),
  ];

  get version() {
    return this.config.version;
  }

  checkDependency(name: string, hasNoBounds: boolean): string[] {
    if (name.startsWith(this.config.name)) {
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
    Object.keys(this.config.plan).sort((a, b) => Version.compare(b, a));

  write = () => {
    const { rules, bounds, ...fields } = this.config;

    return ConfigYaml.write(
      PATH,
      {
        ...fields,
        bounds: formatBounds(bounds),
        rules: map<Rules, Rules<true>>(formatRule, rules),
      },
      {
        sortKeys: compareConfigKeys,
        lineWidth: 240,
        condenseFlow: true,
      }
    );
  };

  update = ({ next, prev, isBreaking }: VersionUpdate): Config => {
    const { version, bounds, ...rest } = this.config;

    if (prev !== version) {
      throw new Error(`invalid versions ${version} and ${prev}`);
    }

    const newBounds: Bounds = isBreaking
      ? [next, new Version(next).next(true).format()]
      : bounds;

    return new Config({
      ...rest,
      bounds: newBounds,
      version: next,
    });
  };
}
