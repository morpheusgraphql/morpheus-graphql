import { getInput, setFailed } from "@actions/core";
import { load } from "js-yaml";
import { promisify } from "util";
import { exec } from "child_process";
import { readFile } from "fs";
import path from "path";

type StackPackage = {
  name: string;
};

const checkPackage = async (name: string) => {
  const fileUrl = path.join(name, "package.yaml");
  const file = await promisify(readFile)(fileUrl, "utf8");
  const pkg = load(file) as StackPackage;

  console.log(file, pkg);
};

const uploadPackage = async (name: string) => {
  const packageName = `morpheus-graphql-${name}`;

  console.info(`start uploading package ${packageName}!`);

  // check package version number
  await checkPackage(packageName);

  // upload package
  await promisify(exec)(`stack upload ${packageName}`);

  console.info(`successfully uploaded package${packageName}!`);
};

uploadPackage(getInput("name")).catch(({ message }) => setFailed(message));
