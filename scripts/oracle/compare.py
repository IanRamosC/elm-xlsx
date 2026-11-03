"""Compare Xlsx.Read with python-calamine on a set of .xlsx files.

Usage (from the repo root):
    scripts/.venv/bin/python -I scripts/oracle/compare.py FILE.xlsx...

Runs scripts/oracle/dump.mjs (Node 24 must be on PATH), then checks every
cell of every worksheet against calamine. Known differences that are not
bugs are allowed:
  - calamine reports error cells and formulas without a cached value as "".
  - calamine returns durations ([h]:mm:ss) as timedelta; we return days.
  - calamine returns time-only values as time; we return a date-time on the
    epoch day, so only the time of day is compared. It also returns serial 0
    in a date format as time(0, 0), where we return the epoch date.
  - Sheets calamine cannot open (it does not decode percent-encoded part
    names, for instance) are reported but not counted as failures.
"""

import datetime as dt
import json
import math
import os
import subprocess
import sys
import tempfile

from python_calamine import CalamineWorkbook

HERE = os.path.dirname(os.path.abspath(__file__))


def col_name(n):
    s = ""
    while n:
        n, r = divmod(n - 1, 26)
        s = chr(65 + r) + s
    return s


def same(ours, theirs):
    kind, value = ours["type"], ours["value"]
    if kind in ("error", "empty"):
        return theirs == "" or theirs is None
    if kind == "text":
        return theirs == value
    if kind == "bool":
        return theirs is value
    if kind == "number":
        if isinstance(theirs, dt.timedelta):
            theirs = theirs.total_seconds() / 86400
        if isinstance(theirs, bool) or not isinstance(theirs, (int, float)):
            return False
        return math.isclose(value, theirs, rel_tol=1e-12, abs_tol=1e-300)
    if kind == "date":
        if isinstance(theirs, dt.time) and not isinstance(theirs, dt.datetime):
            return theirs == dt.time(0) and value in ("1899-12-31", "1904-01-01")
        if isinstance(theirs, dt.datetime):
            return theirs.time() == dt.time(0) and theirs.date().isoformat() == value
        return isinstance(theirs, dt.date) and theirs.isoformat() == value
    if kind == "datetime":
        ours_dt = dt.datetime.fromisoformat(value)
        if isinstance(theirs, dt.time):
            return abs(dt.datetime.combine(dt.date(2000, 1, 1), theirs) - dt.datetime.combine(dt.date(2000, 1, 1), ours_dt.time())) <= dt.timedelta(milliseconds=1)
        if isinstance(theirs, dt.datetime):
            return abs(theirs - ours_dt) <= dt.timedelta(milliseconds=1)
        if isinstance(theirs, dt.date):
            return ours_dt.time() == dt.time(0) and ours_dt.date() == theirs
        return False
    return False


def main(paths):
    with tempfile.TemporaryDirectory() as tmp:
        out = os.path.join(tmp, "dump.json")
        subprocess.run(["node", os.path.join(HERE, "dump.mjs"), out, *paths], check=True)
        with open(out) as f:
            dumps = json.load(f)

    failures = 0
    for entry in dumps:
        path, result = entry["name"], entry["result"]
        problems = []
        if "error" in result:
            problems.append(f"could not read: {result['error']}")
        else:
            try:
                wb = CalamineWorkbook.from_path(path)
            except Exception as e:  # noqa: BLE001
                print(f"{os.path.basename(path)}: not compared, calamine cannot open it ({type(e).__name__}: {e})")
                continue
            if [s["name"] for s in result["sheets"]] != wb.sheet_names:
                problems.append(f"sheet names {[s['name'] for s in result['sheets']]} != {wb.sheet_names}")
            for s in result["sheets"]:
                content = s["content"]
                if "error" in content:
                    continue  # chart sheets etc.
                try:
                    grid = wb.get_sheet_by_name(s["name"]).to_python(skip_empty_area=False)
                except Exception as e:  # noqa: BLE001 - calamine's own limits
                    print(f"    note: calamine cannot read {s['name']!r} ({type(e).__name__}: {e}); not compared")
                    continue
                ours = {c["ref"]: c for c in content["cells"]}
                theirs = {}
                for r, row in enumerate(grid, start=1):
                    for c, v in enumerate(row, start=1):
                        if v != "" and v is not None:
                            theirs[f"{col_name(c)}{r}"] = v
                for ref in sorted(set(ours) | set(theirs)):
                    o = ours.get(ref, {"type": "empty", "value": None})
                    t = theirs.get(ref, "")
                    if not same(o, t):
                        problems.append(f"{s['name']}!{ref}: ours {o['type']} {o['value']!r} vs calamine {t!r}")
        status = "ok" if not problems else f"{len(problems)} difference(s)"
        cells = sum(len(s["content"].get("cells", [])) for s in result.get("sheets", []))
        print(f"{os.path.basename(path)}: {status} ({cells} cells)")
        for p in problems[:10]:
            print(f"    {p}")
        failures += bool(problems)
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main(sys.argv[1:])
