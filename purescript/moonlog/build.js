const esbuild = require("esbuild");
const PurescriptPlugin = require("esbuild-plugin-purescript");

const path = require("path");
const fs = require("fs");
const util = require("util");
const nearley = require("nearley");
const compile = require("nearley/lib/compile");
const generate = require("nearley/lib/generate");
const nearleyGrammar = require("nearley/lib/nearley-language-bootstrapped");

const readFile = (fileName) => util.promisify(fs.readFile)(fileName, "utf8");

function compileGrammar(sourceCode) {
  // Parse the grammar source into an AST
  const grammarParser = new nearley.Parser(nearleyGrammar);
  grammarParser.feed(sourceCode);
  const grammarAst = grammarParser.results[0]; // TODO check for errors

  // Compile the AST into a set of rules
  const grammarInfoObject = compile(grammarAst, {});
  // Generate JavaScript code from the rules
  const grammarJs = generate(grammarInfoObject, "grammar");

  return grammarJs;
}

const NearleyPlugin = () => ({
  name: "nearley",
  setup(build) {
    const namespace = "nearley";
    const fileFilter = /\.ne$/;

    build.onResolve({ filter: fileFilter }, (args) => ({
      path: path.join(args.resolveDir, args.path),
      namespace,
      pluginData: { resolveDir: args.resolveDir },
    }));

    build.onLoad({ filter: /.*/, namespace }, async (args) => {
      try {
        return {
          contents: compileGrammar(await readFile(args.path)),
          resolveDir: args.pluginData.resolveDir,
        };
      } catch (e) {
        console.log(e.token);
        return {
          errors: [
            {
              text: e.message,
            },
          ],
        };
      }
    });
  },
});

esbuild
  .build({
    entryPoints: ["src/index.js"],
    bundle: true,
    outdir: "dist",
    plugins: [PurescriptPlugin(), NearleyPlugin()],
    sourcemap: "inline",
    platform: "node",
  })
  .catch((_e) => process.exit(1));
