import { checkPackages } from "./lib/check-packages";
import { exit } from "./lib/utils/utils";

checkPackages().catch(exit);
