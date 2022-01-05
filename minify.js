const fs = require("fs");
const esbuild = require("esbuild");

const pureFuncs = [
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
];

const elmCode = fs.readFileSync("elm.js", "utf8");

// Remove IIFE.
const nonIIFECode =
  "var scope = window;" +
  elmCode.slice(elmCode.indexOf("{") + 1, elmCode.lastIndexOf("}"));

const result = esbuild.transformSync(nonIIFECode, {
  minify: true,
  pure: pureFuncs,
  target: "es5",
  // This enables top level minification, and re-adds an IIFE.
  format: "iife",
});

fs.writeFileSync("elm.js", result.code, { encoding: "utf8" });
