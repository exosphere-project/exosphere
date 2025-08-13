import * as terser from "terser";

const terserOptions: terser.MinifyOptions = {
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
export default async function postprocess({
  code,
  compilationMode,
  runMode,
}: {
  code: string;
  targetName: string;
  compilationMode: "debug" | "standard" | "optimize";
  runMode: "hot" | "make";
  argv: Array<string>;
}) {
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

  return code;
}
