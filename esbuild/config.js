import path from "node:path";
import { fileURLToPath } from "node:url";
import { wasmLoader } from "esbuild-plugin-wasm";
import { polyfillNode } from "esbuild-plugin-polyfill-node";

const isBrowser = !!process.env.BROWSER_RUNTIME;
const projectRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "..",
);

export const buildOptions = ({ entryPoint, outfile }) => {
  const config = {
    entryPoints: [entryPoint],
    outfile: outfile,
    absWorkingDir: projectRoot,
    define: {
      BROWSER_RUNTIME: isBrowser ? "true" : '""',
    },
    plugins: [
      wasmLoader({
        mode: "embedded", // deferred?
      }),
    ],
    bundle: true,
    platform: isBrowser ? "browser" : "node",
    format: "esm",
    treeShaking: true,
    logLevel: "error",
  };

  // https://esbuild.github.io/api/#packages
  if (!isBrowser) {
    config.packages = "external";
  } else {
    config.plugins.push(
      polyfillNode({
        polyfills: {
          crypto: true,
          fs: true,
          os: true,
        },
      }),
    );
  }

  return config;
};
