const webpack = require("webpack")
const path = require("path")
const config = require("sapper/config/webpack.js")
const pkg = require("./package.json")
const MiniCssExtractPlugin = require("mini-css-extract-plugin")
const OptimizeCssAssetsPlugin = require("optimize-css-assets-webpack-plugin")
const { resolve } = require("path")

const sourceDir = resolve(__dirname, "src")

const mode = process.env.NODE_ENV || "development"
const dev = mode === "development"

const hotReload = false

const alias = { svelte: path.resolve("node_modules", "svelte") }
const extensions = [".mjs", ".js", ".json", ".svelte", ".html", ".ts"]
const mainFields = ["svelte", "module", "browser", "main"]

const commonPlugins = [
  ...(dev
    ? []
    : [
        new OptimizeCssAssetsPlugin({
          assetNameRegExp: /\.css$/g,
          cssProcessor: require("cssnano"),
          cssProcessorPluginOptions: {
            preset: ["default", { discardComments: { removeAll: true } }]
          },
          canPrint: true
        })
      ]),
  new MiniCssExtractPlugin({
    filename: "[name].css",
    chunkFilename: "[name].[id].css"
  })
]

const commonRules = [
  {
    test: /\.ts$/,
    exclude: resolve(__dirname, "node_modules"),
    use: "babel-loader"
  }
]

const sassRule = server => ({
  test: /\.(sa|sc|c)ss$/,
  use: [
    MiniCssExtractPlugin.loader,
    {
      loader: "css-loader",
      options: {
        importLoaders: 1
      }
    },
    {
      loader: "sass-loader",
      options: {
        sassOptions: { includePaths: ["./src", "./node_modules"] }
      }
    }
  ]
})

module.exports = {
  client: {
    entry: resolve(sourceDir, "client.ts"),
    output: {
      ...config.client.output(),
      filename: "client.js"
    },
    resolve: { alias, extensions, mainFields },
    module: {
      rules: [
        ...commonRules,
        sassRule(false),
        {
          test: /\.(svelte|html)$/,
          use: {
            loader: "svelte-loader",
            options: {
              dev,
              hydratable: true,
              hotReload
            }
          }
        }
      ]
    },
    mode,
    plugins: [
      hotReload && dev && new webpack.HotModuleReplacementPlugin(),
      new webpack.DefinePlugin({
        "process.browser": true,
        "process.env.NODE_ENV": JSON.stringify(mode)
      }),
      ...commonPlugins
    ].filter(Boolean),
    devtool: dev && "inline-source-map"
  },

  server: {
    entry: resolve(sourceDir, "server.ts"),
    output: {
      ...config.server.output(),
      filename: "server.js"
    },
    target: "node",
    resolve: { alias, extensions, mainFields },
    externals: Object.keys(pkg.dependencies).concat("encoding"),
    module: {
      rules: [
        ...commonRules,
        sassRule(true),
        {
          test: /\.(svelte|html)$/,
          use: {
            loader: "svelte-loader",
            options: {
              css: false,
              generate: "ssr",
              dev
            }
          }
        }
      ]
    },
    plugins: [...commonPlugins],
    mode,
    performance: {
      hints: false // it doesn't matter if server.js is large
    },
    node: {
      __dirname: false
    }
  },

  serviceworker: {
    entry: config.serviceworker.entry(),
    output: config.serviceworker.output(),
    mode
  }
}
