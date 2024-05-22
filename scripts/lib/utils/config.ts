import { Yaml } from "./file";
import { StrVersion } from "./version";

type Configuration = { version: StrVersion };

const file = new Yaml<Configuration, []>(() => "./hconf.yaml");

export const getVersion = () => file.read().then((x) => x.version);
