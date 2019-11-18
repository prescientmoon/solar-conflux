import { resolve } from 'path'
import { readFileSync, writeFileSync } from 'fs'

const path = resolve(__dirname, '../package.json')

const npmConfig = JSON.parse(readFileSync(path).toString()) as {
  name: string
}

npmConfig.name = npmConfig.name.substr(npmConfig.name.indexOf('/') + 1)

writeFileSync(path, JSON.stringify(npmConfig))
