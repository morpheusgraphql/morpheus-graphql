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
import { join } from "path";
import { defs } from "./defs";

export type StackPlan = {
  extra?: Dict<PkgName>;
  resolver: string;
  include?: PkgName[];
  exclude?: PkgName[];
};

type Pkg = {
  names: string[];
  dir: string;
  prefix?: string;
};

type Configuration<R extends boolean = false> = {
  name: string;
  version: StrVersion;
  bounds: Bounds<R>;
  dependencies: Rules<R>;
  builds: Dict<StackPlan>;
  packages: [Pkg];
};

const ORDER = ["name", "version", "bounds", "packages"].reverse();

const sortKeys = (a: string, b: string) => {
  try {
    return Version.compare(a, b);
  } catch {
    const x = a.toLowerCase();
    const y = b.toLowerCase();

    const order = ORDER.indexOf(y) - ORDER.indexOf(x);

    if (order !== 0) {
      return Math.sign(order);
    }

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

const file = new Yaml<Configuration<true>, []>(() => defs.CONFIG);

const withPrefix = (prefix: string, s: string) =>
  s === "." ? prefix : `${prefix}-${s}`;

export class Config {
  constructor(private config: Configuration) {}

  static load = async (change?: VersionUpdate) => {
    const { dependencies, bounds, ...rest } = await file.read();

    const config = new Config({
      ...rest,
      bounds: parseBound(bounds),
      dependencies: map<Rules<true>, Rules>(parseRule, dependencies),
    });

    return change ? config.update(change) : config;
  };

  packages = () => {
    const { packages } = this.config;

    return packages.flatMap(({ dir, names, prefix }) =>
      names.map((s) => join(dir, prefix?.length ? withPrefix(prefix, s) : s))
    );
  };

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
      this.config.builds[version],
      `ghc version ${version} is not supported! supported versions are: \n - ${this.plans().join(
        "\n - "
      )}`
    );

  rule = (name: string) => this.config.dependencies[name];

  plans = () =>
    Object.keys(this.config.builds).sort((a, b) => Version.compare(b, a));

  write = () => {
    const { dependencies, bounds, ...fields } = this.config;

    return file.write(
      {
        ...fields,
        bounds: formatBounds(bounds),
        dependencies: map<Rules, Rules<true>>(formatRule, dependencies),
      },
      { sortKeys, lineWidth: 240, condenseFlow: true }
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
