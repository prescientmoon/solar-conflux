const esbuild = require("esbuild");

const production = process.env.NODE_ENV === "production";

esbuild
  .build({
    bundle: true,
    entryPoints: ["src/index.ts"],
    watch: !production,
    outdir: "public/dist",
  })
  .catch((e) => process.exit(0));
