import { resolve } from 'path'
import { readFileSync, writeFileSync } from 'fs'

const path = resolve(__dirname, '../package.json')

const npmConfig = JSON.parse(readFileSync(path).toString()) as {
  name: string
  publishConfig: {
    registry: string
  }
}

npmConfig.name =
  '@Mateiadrielrafael' + npmConfig.name.substr(npmConfig.name.indexOf('/'))

npmConfig.publishConfig = {
  registry: 'https://npm.pkg.github.com/'
}

writeFileSync(path, JSON.stringify(npmConfig))
