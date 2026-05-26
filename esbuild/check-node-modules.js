import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const projectRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "..",
);
const nodeModules = path.join(projectRoot, "node_modules");

export function nodeModulesIsPortable() {
  if (!fs.existsSync(nodeModules)) {
    return false;
  }

  const realNodeModules = fs.realpathSync(nodeModules);
  if (!realNodeModules.startsWith(projectRoot + path.sep)) {
    return false;
  }

  return !realNodeModules.includes(`${path.sep}nix${path.sep}store${path.sep}`);
}

const isMain = process.argv[1] === fileURLToPath(import.meta.url);

if (isMain && !nodeModulesIsPortable()) {
  console.error(
    "node_modules is missing or is the Nix dev shell symlink tree under /nix/store.",
  );
  console.error(
    "Run esbuild outside `nix develop`: rm -rf node_modules && npm ci --ignore-scripts && make bundle-js",
  );
  process.exit(1);
}
