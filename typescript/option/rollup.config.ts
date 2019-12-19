import commonjs from 'rollup-plugin-commonjs'
import nodeResolve from '@rollup/plugin-node-resolve'
import dts from 'rollup-plugin-dts'
import typescript from 'rollup-plugin-typescript2'
import { terser } from 'rollup-plugin-terser'
import { resolve } from 'path'

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
                file: `${outputDirectory}/bundle.cjs.js`,
                format: 'cjs',
                sourcemap: true
            },
            {
                file: `${outputDirectory}/bundle.esm.js`,
                format: 'esm',
                sourcemap: true
            },
            {
                file: `${outputDirectory}/bundle.amd.js`,
                sourcemap: true,
                format: 'amd',
                name: 'Option'
            }
        ],
        plugins: [
            nodeResolve({
                extensions: ['.ts']
            }),
            commonjs(),
            typescript({
                tsconfig: resolve(__dirname, 'tsconfig.json')
            }),
            !dev && terser()
        ]
    },
    {
        input: inputFile,
        output: [{ file: `${outputDirectory}/index.d.ts`, format: 'es' }],
        plugins: [dts()]
    }
]
