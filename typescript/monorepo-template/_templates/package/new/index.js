const packageJson = require("../../../package.json");

console.log(JSON.stringify(packageJson.repository));

exports.prompt = async ({ args }) => ({
  ...packageJson,
  ...args,
  org: packageJson.name,
  repository: JSON.stringify(packageJson.repository),
  bugs: JSON.stringify(packageJson.bugs),
});
