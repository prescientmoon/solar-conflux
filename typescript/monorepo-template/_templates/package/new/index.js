const packageJson = require('../../../package.json')

exports.prompt = async ({ args }) => ({
  ...packageJson,
  ...args,
  org: packageJson.name,
  repository: JSON.stringify(packageJson.repository),
  bugs: JSON.stringify(packageJson.bugs),
  description: args.description || ''
})
