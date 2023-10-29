const esbuild = require("esbuild");
const PurescriptPlugin = require("esbuild-plugin-purescript");

const production = process.env.NODE_ENV === "production";

esbuild
  .build({
    platform: "node",
    entryPoints: ["src/Foreign/blueprint.ts"],
    bundle: true,
    minify: production,
    outdir: "dist",
    watch: true,
    plugins: [PurescriptPlugin()],
    sourcemap: "both",
    target: "es2016",
  })
  .catch((_e) => process.exit(1));
