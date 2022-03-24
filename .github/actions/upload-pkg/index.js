const core = require("@actions/core");
const github = require("@actions/github");

try {
  const name = core.getInput("name");
  const packageName = `morpheus-graphql-${name}`;

  console.info(`start uploading package ${packageName}!`);
  const payload = JSON.stringify(github.context.payload, undefined, 2);

  console.log(`The event payload: ${payload}`);
} catch (error) {
  core.setFailed(error.message);
}
