import { Yaml } from "./file";

const file = new Yaml<{ version: string }, []>(() => "./hconf.yaml");

export const getVersion = () => file.read().then((x) => x.version);
