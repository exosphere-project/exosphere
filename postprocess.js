import { execSync } from "node:child_process";
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

/**
 * @type {import("elm-watch/elm-watch-node").Postprocess}
 */
export default async function postprocess({ code, compilationMode, runMode }) {
  if (compilationMode === "optimize" && runMode === "make") {
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
    } catch (e) {
      console.warn("Could not get git status for versioning.");
      throw e;
    }
  }

  return code;
}
