import { terser } from 'rollup-plugin-terser'
import { resolve } from 'path'
import ts from '@wessberg/rollup-plugin-ts'
import nodeResolve from '@rollup/plugin-node-resolve'
import commonjs from '@rollup/plugin-commonjs'
import filesize from 'rollup-plugin-filesize'

const packageRoot = process.cwd()
const dev = Boolean(process.env.ROLLUP_WATCH)

const inputFile = resolve(packageRoot, 'src/index.ts')

const _package = require(resolve(packageRoot, 'package.json'))
const packageName = String(_package.name)

const commonPlugins = [nodeResolve(), commonjs()]

export default [
  {
    input: inputFile,
    output: [
      {
        file: _package.main,
        format: 'cjs',
        sourcemap: true
      },
      {
        file: _package.browser,
        sourcemap: true,
        format: 'umd',
        name: packageName[0].toUpperCase() + packageName.substr(1)
      }
    ],
    plugins: [...commonPlugins, ts(), !dev && terser()]
  },
  {
    input: inputFile,
    output: [
      {
        file: _package.module,
        format: 'esm',
        sourcemap: true
      }
    ],
    plugins: [
      ...commonPlugins,
      ts({
        tsconfig: {
          declaration: true,
          ...require(resolve(__dirname, 'tsconfig.json'))['compilerOptions']
        }
      }),
      !dev && terser(),
      filesize({
        showBrotliSize: true
      })
    ]
  }
]
