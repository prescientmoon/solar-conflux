const esbuild = require("esbuild");
const PurescriptPlugin = require("esbuild-plugin-purescript");
const path = require("path");

const isProd = process.env.NODE_ENV === "production";

esbuild
  .build({
    entryPoints: ["src/index.js"],
    bundle: true,
    outdir: "dist",
    watch: !isProd,
    plugins: [
      PurescriptPlugin({
        output: isProd ? path.resolve(__dirname, "dce-output") : undefined,
      }),
    ],
    define: {
      global: "window",
    },
  })
  .catch((_e) => process.exit(1));
