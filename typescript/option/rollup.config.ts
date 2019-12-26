import { terser } from 'rollup-plugin-terser'
import { resolve } from 'path'
import ts from '@wessberg/rollup-plugin-ts'
import nodeResolve from '@rollup/plugin-node-resolve'
import commonjs from '@rollup/plugin-commonjs'
import _package from './package.json'
import filesize from 'rollup-plugin-filesize'

const inputFile = resolve(__dirname, 'src/index.ts')

const dev = Boolean(process.env.ROLLUP_WATCH)

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
                name: 'Option'
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
                    ...require(resolve(__dirname, 'tsconfig.json'))[
                        'compilerOptions'
                    ]
                }
            }),
            !dev && terser(),
            filesize({
                showBrotliSize: true
            })
        ]
    }
]
