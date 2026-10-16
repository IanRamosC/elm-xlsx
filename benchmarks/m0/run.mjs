// M0 spike runner: node run.mjs [cellsForWriteAndScan] [cellsForDeflate]
// Build first: elm make src/Main.elm --optimize --output=elm.js
import { createRequire } from "node:module";
import { writeFileSync } from "node:fs";
import { performance } from "node:perf_hooks";

const require = createRequire(import.meta.url);
const { Elm } = require("./elm.js");

const small = Number(process.argv[2] ?? 100_000);
const large = Number(process.argv[3] ?? 300_000);
const app = Elm.Main.init();

let pending = null;
app.ports.done.subscribe((result) => pending(result));

function run(cmd) {
  return new Promise((resolve) => {
    const start = performance.now();
    pending = (result) => resolve({ ms: performance.now() - start, result });
    app.ports.run.send(cmd);
  });
}

async function step(label, cmd, targetMs) {
  const { ms, result } = await run(cmd);
  const { file, ...shown } = result;
  const verdict = targetMs ? (ms <= targetMs ? "PASS" : "FAIL") + ` (target ${targetMs} ms)` : "";
  console.log(`${label.padEnd(34)} ${ms.toFixed(0).padStart(7)} ms  ${verdict}  ${JSON.stringify(shown)}`);
  return result;
}

console.log(`Node ${process.version}\n`);
console.log(`-- ${small.toLocaleString()} cells`);
await step("write: build XML string", `gen:${small}`);
await step("write: encode to bytes", "encode");
await step("scan: elm/parser", "scan:parser", 2000);
await step("scan: String.split", "scan:split", 2000);
await step("write: deflate (dynamic)", "deflate:dynamic");

console.log(`\n-- ${large.toLocaleString()} cells (~10 MB of sheet XML)`);
await step("build XML string", `gen:${large}`);
await step("encode to bytes", "encode");
await step("deflate (raw / store)", "deflate:raw");
await step("deflate (static huffman)", "deflate:static");
await step("deflate (dynamic, elm-zip default)", "deflate:dynamic", 4000);
await step("deflate (dynamic, window 1024)", "deflate:dynamic:1024", 4000);
await step("deflate (dynamic, window 4096)", "deflate:dynamic:4096", 4000);
await step("inflate", "inflate", 2000);
await step("decode bytes to string", "decode");

const { file } = await step("\none-cell workbook", "onecell");
writeFileSync("one-cell.xlsx", Buffer.from(file));
console.log("wrote one-cell.xlsx");
