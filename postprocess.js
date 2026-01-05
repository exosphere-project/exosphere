import { execSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import vm from "node:vm";
import * as terser from "terser";

/**
 * @type {terser.MinifyOptions}
 */
const terserOptions = {
  sourceMap: false,
  mangle: true,
  compress: {
    pure_funcs: [
      "F2",
      "F3",
      "F4",
      "F5",
      "F6",
      "F7",
      "F8",
      "F9",
      "A2",
      "A3",
      "A4",
      "A5",
      "A6",
      "A7",
      "A8",
      "A9",
    ],
    pure_getters: true,
    keep_fargs: false,
    unsafe_comps: true,
    unsafe: true,
  },
};

const writeVersionArtifact = () => {
  try {
    const root = process.cwd();

    // We can't import config.js because it is not a module. It needs to be evaluated in a sandbox.
    const configPath = path.join(root, "config.js");
    const src = fs.readFileSync(configPath, "utf8");
    const sandbox = {};
    vm.createContext(sandbox);
    vm.runInContext(src, sandbox, { filename: "config.js" });
    const version = sandbox.config?.version;

    // Write version.json.
    const payload = JSON.stringify({ version }, null, 2);
    fs.writeFileSync(path.join(root, "version.json"), payload, "utf8");
  } catch (error) {
    console.warn("Could not generate version artifact:", error);
  }
};

/**
 * @type {import("elm-watch/elm-watch-node").Postprocess}
 */
export default async function postprocess({ code, compilationMode, runMode }) {
  if (runMode === "make") {
    writeVersionArtifact();
  }

  if (compilationMode === "optimize") {
    code = await terser
      .minify({ "elm-web.js": code }, terserOptions)
      .then((result) => {
        if (result.code == undefined) {
          throw new Error("Failed to minify with terser");
        }
        return result.code;
      });
  }

  if (compilationMode === "debug") {
    code += `\nwindow.__DEBUG__ = true;\n`;
  }

  if (runMode === "hot") {
    try {
      const gitHash = execSync("git rev-parse --short HEAD", {
        encoding: "utf8",
      }).trim();
      const uncommittedChanges = execSync("git status --porcelain", {
        encoding: "utf8",
      }).trim();
      const version = `${gitHash}${uncommittedChanges ? "*" : ""}`;
      code += `\nwindow.__VERSION__ = "${version}";\n`;
    } catch (_) {
      console.warn("Could not get git status for versioning.");
    }
  }

  return code;
}
