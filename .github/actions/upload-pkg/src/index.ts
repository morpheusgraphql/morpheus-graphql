import { getInput, setFailed } from "@actions/core";

import { uploadPackage } from "./upload-package";

uploadPackage(getInput("name")).catch(({ message }) => setFailed(message));
