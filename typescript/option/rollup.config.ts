import { terser } from 'rollup-plugin-terser'
import { resolve } from 'path'
import ts from '@wessberg/rollup-plugin-ts'

const outputDirectory = resolve(__dirname, 'dist')
const inputFile = resolve(__dirname, 'src/index.ts')

const npmConfig = require(resolve(__dirname, `package.json`))

const external = Object.keys(npmConfig.dependencies || {})
const dev = Boolean(process.env.ROLLUP_WATCH)

export default [
    {
        input: inputFile,
        external,
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
        plugins: [ts(), !dev && terser()]
    },
    {
        input: inputFile,
        external,
        output: [
            {
                file: `${outputDirectory}/index.esm.js`,
                format: 'esm',
                sourcemap: true
            }
        ],
        plugins: [
            ts({
                tsconfig: {
                    declaration: true
                }
            }),
            !dev && terser()
        ]
    }
]
