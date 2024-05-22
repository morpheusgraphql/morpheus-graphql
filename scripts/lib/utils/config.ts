import { Yaml } from "./file";
import { VersionUpdate, Version, StrVersion } from "./version";
import { Bounds, formatBounds, parseBound } from "./rule";

type Configuration<R extends boolean = false> = {
  version: StrVersion;
  bounds: Bounds<R>;
};

const file = new Yaml<Configuration<true>, []>(() => "./hconf.yaml");

export class Config {
  constructor(private config: Configuration) {}

  static load = async (change?: VersionUpdate) => {
    const { bounds, ...rest } = await file.read();

    const config = new Config({ ...rest, bounds: parseBound(bounds) });

    return change ? config.update(change) : config;
  };

  get version() {
    return this.config.version;
  }

  write = () => {
    const { bounds, ...fields } = this.config;

    return file.write(
      { bounds: formatBounds(bounds), ...fields },
      { lineWidth: 240 }
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
