import { defineConfig } from "vite";
import preact from "@preact/preset-vite";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [preact()],
  assetsInclude: [__dirname + "/public/assets/**/*.{png}"],
  base: "./",
});
