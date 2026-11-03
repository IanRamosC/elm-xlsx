// Read .xlsx files with Xlsx.Read and print the cells as JSON.
// Usage (from the repo root): node scripts/oracle/dump.mjs out.json file.xlsx...
import { createRequire } from "node:module";
import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { basename, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const samples = join(here, "..", "samples");
const elmJs = join(here, "elm.js");

execFileSync("elm", ["make", "src/Dump.elm", "--optimize", `--output=${elmJs}`], {
  cwd: samples,
  stdio: ["ignore", "ignore", "inherit"],
});

const [outPath, ...paths] = process.argv.slice(2);
const require = createRequire(import.meta.url);
const { Elm } = require(elmJs);
const app = Elm.Dump.init();
app.ports.dump.subscribe((result) => {
  writeFileSync(outPath, JSON.stringify(result, null, 1));
});
app.ports.files.send(paths.map((p) => ({ name: p, base64: readFileSync(p).toString("base64") })));
