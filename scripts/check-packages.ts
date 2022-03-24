import { uploadPackage } from "./upload-package";

const packages = [
  "tests",
  "core",
  "code-gen",
  "app",
  "client",
  "subscriptions",
  "",
];

const main = () =>
  Promise.all(packages.map(uploadPackage)).catch((err) => {
    console.error(err.message);
    process.exit(1);
  });

main();
