import { defineConfig } from "vite";
import preact from "@preact/preset-vite";
import glsl from "vite-plugin-glsl";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [preact(), glsl()],
  assetsInclude: [__dirname + "/public/assets/**/*.{png}"],
  base: "./",
});
