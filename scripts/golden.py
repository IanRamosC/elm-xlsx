"""Compare generated workbooks with Excel-made reference files, part by part.

This is XlsxWriter's testing method: each reference in
tests/fixtures/xlsxwriter/ was saved by Excel, and the matching sample in
scripts/out/ must contain the same XML once both are normalized (attribute
order and whitespace ignored, document timestamps and author ignored).

Usage (from the repo root, after `node scripts/samples/run.mjs`):
    scripts/.venv/bin/python -I scripts/golden.py

Differences we make on purpose are listed in ALLOWED and explained there.
Exits with status 1 if any other difference is found.
"""

import difflib
import sys
import xml.etree.ElementTree as ET
import zipfile
from pathlib import Path

ROOT = Path(__file__).parent.parent
OUT = ROOT / "scripts" / "out"
REFERENCES = ROOT / "tests" / "fixtures" / "xlsxwriter"

# sample name in scripts/out -> reference file name
PAIRS = {"simple": "simple01"}

# Parts we never write, on purpose.
SKIPPED_PARTS = {
    # No theme: the default font is set explicitly instead (see the plan).
    "xl/theme/theme1.xml",
}

# Parts whose children are an unordered set.
UNORDERED_PARTS = {"[Content_Types].xml", "_rels/.rels", "xl/_rels/workbook.xml.rels"}

# Elements whose text varies between runs or authors.
IGNORED_TEXT = {"created", "modified", "creator", "lastModifiedBy"}

# (part, normalized line) pairs that may differ, on purpose.
ALLOWED = {
    # Without a theme we cannot reference theme colors or font schemes, so the
    # default font is spelled out: black, Calibri 11, family 2, no scheme.
    ("xl/styles.xml", '<color theme="1"/>'),
    ("xl/styles.xml", '<color rgb="FF000000"/>'),
    ("xl/styles.xml", '<scheme val="minor"/>'),
    # The theme relationship and content type go with the theme part.
    ("xl/_rels/workbook.xml.rels", '<Relationship Id="rId2" Target="theme/theme1.xml" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme"/>'),
    ("xl/_rels/workbook.xml.rels", '<Relationship Id="rId2" Target="styles.xml" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles"/>'),
    ("xl/_rels/workbook.xml.rels", '<Relationship Id="rId3" Target="styles.xml" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles"/>'),
    ("xl/_rels/workbook.xml.rels", '<Relationship Id="rId3" Target="sharedStrings.xml" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings"/>'),
    ("xl/_rels/workbook.xml.rels", '<Relationship Id="rId4" Target="sharedStrings.xml" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings"/>'),
    ("[Content_Types].xml", '<Override ContentType="application/vnd.openxmlformats-officedocument.theme+xml" PartName="/xl/theme/theme1.xml"/>'),
    # Excel writes its own build number; we write XlsxWriter's.
    ("xl/workbook.xml", '<fileVersion appName="xl" lastEdited="4" lowestEdited="4" rupBuild="4505"/>'),
    ("xl/workbook.xml", '<workbookPr defaultThemeVersion="124226"/>'),
    ("xl/workbook.xml", "<workbookPr/>"),
}


def local(tag):
    return tag.split("}", 1)[-1]


def normalize(xml_bytes):
    """One line per element: '<tag a="1" b="2">text' with namespaces dropped
    from tags, attributes sorted, and closing tags kept so structure shows."""
    root = ET.fromstring(xml_bytes)
    lines = []

    def walk(el, depth):
        tag = local(el.tag)
        attrs = " ".join(f'{local(k)}="{v}"' for k, v in sorted(el.attrib.items(), key=lambda kv: local(kv[0])))
        opening = f"<{tag}{' ' + attrs if attrs else ''}"
        text = (el.text or "") if tag not in IGNORED_TEXT else "…"
        children = list(el)
        if not children and not text.strip():
            lines.append(opening + "/>")
            return
        lines.append(opening + ">" + text)
        for child in children:
            walk(child, depth + 1)
        lines.append(f"</{tag}>")

    walk(root, 0)
    return lines


def parts(path):
    with zipfile.ZipFile(path) as z:
        return {name: z.read(name) for name in z.namelist() if not name.endswith("/")}


def compare(sample, reference):
    ours = parts(OUT / f"{sample}.xlsx")
    theirs = parts(REFERENCES / f"{reference}.xlsx")
    problems = []

    for name in sorted(set(ours) | set(theirs)):
        if name in SKIPPED_PARTS:
            continue
        if name not in ours:
            problems.append(f"missing part {name}")
            continue
        if name not in theirs:
            problems.append(f"extra part {name}")
            continue
        a, b = normalize(theirs[name]), normalize(ours[name])
        if name in UNORDERED_PARTS:
            a, b = sorted(a), sorted(b)
        diff = [
            line
            for line in difflib.unified_diff(a, b, "excel/" + name, "elm/" + name, lineterm="", n=0)
            if not line.startswith(("---", "+++", "@@"))
        ]
        unexpected = [line for line in diff if (name, line[1:]) not in ALLOWED]
        if unexpected:
            problems.append(f"{name}:\n    " + "\n    ".join(unexpected))

    return problems


def main():
    failed = False
    for sample, reference in PAIRS.items():
        problems = compare(sample, reference)
        if problems:
            failed = True
            print(f"{sample} vs {reference}: DIFFERENT")
            for p in problems:
                print("  " + p)
        else:
            print(f"{sample} vs {reference}: same")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
