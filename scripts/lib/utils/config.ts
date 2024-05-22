import { Yaml } from "./file";
import { StrVersion } from "./version";
import { Bounds, parseBound } from "./rule";

type Configuration<R extends boolean = false> = {
  version: StrVersion;
  bounds: Bounds<R>;
};

const file = new Yaml<Configuration<true>, []>(() => "./hconf.yaml");

export class Config {
  constructor(private config: Configuration) {}

  static load = async () => {
    const { bounds, ...rest } = await file.read();
    return new Config({ ...rest, bounds: parseBound(bounds) });
  };

  get version() {
    return this.config.version;
  }
}
