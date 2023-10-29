const esbuild = require("esbuild");

const PurescriptPlugin = require("esbuild-plugin-purescript");
const alias = require("esbuild-plugin-alias");
const { sassPlugin } = require("esbuild-sass-plugin");

const production = process.env.NODE_ENV === "production";
// const preactCompat = require.resolve("preact/compat");

esbuild
  .build({
    entryPoints: ["src/index.ts"],
    bundle: true,
    minify: production,
    outdir: "dist",
    watch: true,
    plugins: [
      PurescriptPlugin(),
      sassPlugin(),
      alias({
        //react: preactCompat,
        //"react-dom": preactCompat,
      }),
    ],
    sourcemap: "both",
    jsxFactory: "h",
    jsxFragment: "Fragment",
    // inject: ["./src/preact-shim.ts"],
    loader: {
      ".svg": "file",
      ".png": "file",
    },
  })
  .catch((_e) => process.exit(1));

