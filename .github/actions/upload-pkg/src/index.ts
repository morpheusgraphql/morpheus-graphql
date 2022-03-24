import { getInput, setFailed } from "@actions/core";
// const github = require("@actions/github");
import { promisify } from "util";
import { exec } from "child_process";
import { readFile } from "fs";
import path from "path";

const checkPackage = async (name: string) => {
  const fileUrl = path.join(name, "package.yaml");
  const file = await promisify(readFile)(fileUrl);

  console.log(file);
};

const uploadPackage = async () => {
  const name = getInput("name");
  const packageName = `morpheus-graphql-${name}`;

  console.info(`start uploading package ${packageName}!`);
  // check package version number
  await checkPackage(packageName);
  // upload package
  await promisify(exec)(`stack upload ${packageName}`);
  console.info(`successfully uploaded package${packageName}!`);
};

uploadPackage().catch(({ message }) => setFailed(message));
