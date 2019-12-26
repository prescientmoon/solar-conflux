import { terser } from 'rollup-plugin-terser'
import { resolve } from 'path'
import ts from '@wessberg/rollup-plugin-ts'
import nodeResolve from '@rollup/plugin-node-resolve'
import commonjs from '@rollup/plugin-commonjs'

const outputDirectory = resolve(__dirname, 'dist')
const inputFile = resolve(__dirname, 'src/index.ts')

const dev = Boolean(process.env.ROLLUP_WATCH)

const commonPlugins = [commonjs(), nodeResolve()]

export default [
    {
        input: inputFile,
        output: [
            {
                file: `${outputDirectory}/index.cjs.js`,
                format: 'cjs',
                sourcemap: true
            },
            {
                file: `${outputDirectory}/index.amd.js`,
                sourcemap: true,
                format: 'amd',
                name: 'Option'
            }
        ],
        plugins: [...commonPlugins, ts(), !dev && terser()]
    },
    {
        input: inputFile,
        output: [
            {
                file: `${outputDirectory}/index.esm.js`,
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
            !dev && terser()
        ]
    }
]
