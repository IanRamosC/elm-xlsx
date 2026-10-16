# M0 spike results

Run on 2026-10-16 with `benchmarks/m0` (`elm make --optimize`, Node 24.22.0, V8),
Elm 0.19.3, `folkertdev/elm-flate` 2.0.6, on an Apple M3 Max. That machine is
faster than the "mid-range laptop" in the plan, so treat these as best-case
numbers and expect roughly 2× on a typical laptop. Still to do: re-run in Chrome
before release.

Reproduce:

```sh
cd benchmarks/m0
elm make src/Main.elm --optimize --output=elm.js
node run.mjs            # optional args: cellsForScan cellsForDeflate
```

## 100,000 cells (3.4 MB of sheet XML)

| Step | Time | Target |
|---|---|---|
| Build XML (reversed `List String` + `String.concat`) | 136 ms | |
| `Bytes.Encode.string` | 26 ms | |
| Deflate (dynamic Huffman, elm-zip default) | 1,786 ms | |
| **Write total** | **≈ 1.95 s** | < 3 s ✅ |
| Scan with `elm/parser` (`chompUntil` loop) | 155 ms | < 2 s ✅ |
| Scan with `String.split` + slices | 41 ms | < 2 s ✅ |

## 300,000 cells (10.7 MB of sheet XML)

| Step | Time | Output | Target |
|---|---|---|---|
| Deflate, raw (stored blocks) | 814 ms | 10.7 MB | |
| Deflate, static Huffman | 5,207 ms | 2.9 MB | |
| Deflate, dynamic, window 1,024 | 5,357 ms | 2.0 MB | |
| Deflate, dynamic, window 4,096 | 5,223 ms | 2.0 MB | |
| **Deflate, dynamic, max window (elm-zip default)** | **5,193 ms** | 2.0 MB | < 4 s ❌ |
| **Inflate** | **1,220 ms** | | < 2 s ✅ |
| `Bytes.Decode.string` (10.7 MB) | 439 ms | | |

Node's default stack size was enough for all of this (no `--stack-size` flag).

## Decisions

1. **Deflate misses its target (≈ 2 MB/s, and the window size makes no difference),
   so the plan's fallback applies:** the writer's default compression deflates a
   part only when it is smaller than a threshold and stores larger parts.
   Proposed threshold: **4 MB of uncompressed XML** (about 2 s to deflate here). Users
   can force `Deflate` or `Store`. A stored ZIP entry is still a valid `.xlsx`.
   elm-zip's `Zip.Entry.store` skips Flate entirely, so the cost of storing is
   one CRC-32 pass.
2. **Scanner: `String.split` + slices** is 4× faster than `elm/parser` and simpler
   to keep stack-safe, so `Internal.SheetScanner` uses it. `elm/parser` is still
   fine for small parts.
3. **String building** as reversed `List String` → `String.concat` →
   one `Bytes.Encode.string` is fast enough (≈ 160 ms per 100k cells), so no
   alternative is needed.
4. **Inflate** passes, but with less headroom on slower machines. Reading
   inflates only the parts it needs, one sheet at a time, as planned.

## Compatibility check

`run.mjs` also writes `one-cell.xlsx` (inline string "Hello" in A1, 42 in B1;
six parts, no docProps). Python's `zipfile.testzip()` accepts it. **Still to do:
open it in Excel and confirm there is no repair prompt.**
