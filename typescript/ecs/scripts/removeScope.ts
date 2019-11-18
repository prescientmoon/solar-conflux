import { resolve } from 'path'
import { readFileSync, writeFileSync } from 'fs'

const path = resolve(__dirname, '../package.json')

const npmConfig = JSON.parse(readFileSync(path).toString()) as {
  name: string
}

npmConfig.name =
  '@Mateiadrielrafael' + npmConfig.name.substr(npmConfig.name.indexOf('/'))

writeFileSync(path, JSON.stringify(npmConfig))
