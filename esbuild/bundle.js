import fs from "node:fs";
import * as esbuild from "esbuild";
import { buildOptions } from "./config.js";

if (process.argv.length < 4) {
  throw `usage: node bundle.js ENTRY_POINT OUTPUT_FILENAME`;
}

const entryPoint = process.argv[2];
const outfile = process.argv[3];

await esbuild.build(
  buildOptions({
    entryPoint,
    outfile,
  }),
);

const bundle = fs.readFileSync(outfile, "utf8");
if (bundle.includes("/nix/store")) {
  throw new Error(
    [
      "Bundle contains /nix/store paths and is not safe to publish or vendor.",
      "Use project-local node_modules before bundling:",
      "  rm -rf node_modules && npm ci --ignore-scripts",
      "  make bundle",
    ].join("\n"),
  );
}
