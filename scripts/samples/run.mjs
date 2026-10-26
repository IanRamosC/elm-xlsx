// Write every sample workbook to scripts/out/.
// Usage (from the repo root): node scripts/samples/run.mjs
import { createRequire } from "node:module";
import { execFileSync } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const out = join(here, "..", "out");
const elmJs = join(here, "elm.js");

execFileSync("elm", ["make", "src/Samples.elm", "--optimize", `--output=${elmJs}`], {
  cwd: here,
  stdio: ["ignore", "ignore", "inherit"],
});

const require = createRequire(import.meta.url);
const { Elm } = require(elmJs);
mkdirSync(out, { recursive: true });

let failed = false;
const app = Elm.Samples.init();
app.ports.emit.subscribe((sample) => {
  if (sample.problems) {
    failed = true;
    console.error(`${sample.name}: ${sample.problems.join("; ")}`);
    return;
  }
  const { base64, ...meta } = sample;
  writeFileSync(join(out, `${sample.name}.xlsx`), Buffer.from(base64, "base64"));
  if (!sample.golden) writeFileSync(join(out, `${sample.name}.json`), JSON.stringify(meta, null, 2));
  console.log(`wrote ${sample.name}.xlsx`);
});

app.ports.start.send(null);
setTimeout(() => process.exit(failed ? 1 : 0), 0);
