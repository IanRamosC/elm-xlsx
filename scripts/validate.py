"""Check the sample workbooks in scripts/out/ with openpyxl and python-calamine.

Usage (from the repo root):
    python3 -m venv scripts/.venv
    scripts/.venv/bin/pip install openpyxl python-calamine
    node scripts/samples/run.mjs
    scripts/.venv/bin/python scripts/validate.py

Each <name>.json lists the cells written and what they should read back as.
openpyxl is checked twice: once for formulas, once with data_only=True for
cached formula results. Exits with status 1 on any mismatch.
"""

import datetime as dt
import json
import math
import sys
import zipfile
from pathlib import Path

import openpyxl
from python_calamine import CalamineWorkbook

OUT = Path(__file__).parent / "out"
failures = []


def fail(sample, where, message):
    failures.append(f"{sample}: {where}: {message}")


def same_number(a, b):
    return isinstance(a, (int, float)) and not isinstance(a, bool) and math.isclose(a, b, rel_tol=1e-15, abs_tol=0)


def xstring(text):
    """Apply OOXML _xHHHH_ escaping to control characters, as the writer does."""
    return "".join(f"_x{ord(c):04X}_" if ord(c) < 0x20 and c not in "\t\n\r" else c for c in text)


def parse_datetime(text):
    return dt.datetime.fromisoformat(text)


def check_value(sample, where, expect, actual, reader):
    kind, value = expect["type"], expect.get("value")
    if kind == "string":
        # openpyxl reads an empty shared string as None, and does not decode
        # _xHHHH_ escapes (Excel and calamine do).
        ok = actual == value or (value == "" and actual is None) or (reader == "openpyxl" and actual == xstring(value))
    elif kind == "number":
        ok = same_number(actual, value)
    elif kind == "bool":
        ok = actual is value
    elif kind == "date":
        wanted = dt.date.fromisoformat(value)
        ok = (isinstance(actual, dt.datetime) and actual == dt.datetime.combine(wanted, dt.time())) or actual == wanted
    elif kind == "datetime":
        ok = isinstance(actual, dt.datetime) and actual == parse_datetime(value)
    elif kind == "error":
        # python-calamine's to_python() turns error cells into "", so only
        # openpyxl can check the code.
        ok = actual == value or (reader == "calamine" and actual == "")
    else:
        ok = False
    if not ok:
        fail(sample, where, f"{reader}: expected {kind} {value!r}, got {actual!r}")


def check_sample(meta):
    name = meta["name"]
    path = OUT / f"{name}.xlsx"

    with zipfile.ZipFile(path) as z:
        bad = z.testzip()
        if bad:
            fail(name, "zip", f"corrupt entry {bad}")
        if z.namelist()[0] != "[Content_Types].xml":
            fail(name, "zip", "[Content_Types].xml is not the first entry")

    formulas = openpyxl.load_workbook(path)
    values = openpyxl.load_workbook(path, data_only=True)
    calamine = CalamineWorkbook.from_path(str(path))

    if formulas.sheetnames != meta["sheets"]:
        fail(name, "sheets", f"openpyxl sees {formulas.sheetnames}, expected {meta['sheets']}")
    if calamine.sheet_names != meta["sheets"]:
        fail(name, "sheets", f"calamine sees {calamine.sheet_names}, expected {meta['sheets']}")
    if formulas.active.title != meta["active"]:
        fail(name, "active", f"openpyxl active sheet {formulas.active.title!r}, expected {meta['active']!r}")

    grids = {sheet: calamine.get_sheet_by_name(sheet).to_python(skip_empty_area=False) for sheet in meta["sheets"]}

    for cell in meta["cells"]:
        sheet, ref, expect = cell["sheet"], cell["ref"], cell["expect"]
        where = f"{sheet}!{ref}"
        col, row = openpyxl.utils.cell.coordinate_from_string(ref)
        r, c = row - 1, openpyxl.utils.cell.column_index_from_string(col) - 1
        grid = grids[sheet]
        from_calamine = grid[r][c] if r < len(grid) and c < len(grid[r]) else None

        if expect["type"] == "formula":
            stored = formulas[sheet][ref].value
            if stored != "=" + expect["formula"]:
                fail(name, where, f"openpyxl formula {stored!r}, expected {'=' + expect['formula']!r}")
            cached = expect["cached"]
            if cached is None:
                if values[sheet][ref].value is not None:
                    fail(name, where, f"expected no cached value, got {values[sheet][ref].value!r}")
            else:
                check_value(name, where, cached, values[sheet][ref].value, "openpyxl")
                check_value(name, where, cached, from_calamine, "calamine")
        else:
            check_value(name, where, expect, values[sheet][ref].value, "openpyxl")
            check_value(name, where, expect, from_calamine, "calamine")

    print(f"{name}: {len(meta['cells'])} cells checked")


def main():
    samples = sorted(OUT.glob("*.json"))
    if not samples:
        sys.exit("No samples in scripts/out/. Run: node scripts/samples/run.mjs")
    for meta_path in samples:
        check_sample(json.loads(meta_path.read_text()))
    if failures:
        print("\nFAILED:")
        for f in failures:
            print("  " + f)
        sys.exit(1)
    print("\nAll samples passed.")


if __name__ == "__main__":
    main()
