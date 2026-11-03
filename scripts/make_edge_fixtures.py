"""Write the edge-case fixtures in tests/fixtures/edge/.

Usage (from the repo root):
    scripts/.venv/bin/python -I scripts/make_edge_fixtures.py

Some files come from openpyxl (another real writer); the rest are built by
hand to reproduce what other programs emit: prefixed XML from the .NET Open
XML SDK, ISO 29500 Strict namespaces, cells without `r`, inline strings,
shared formulas, CRLF text, phonetic runs, a chart sheet.
"""

import datetime as dt
import os
import zipfile

from openpyxl import Workbook
from openpyxl.cell.rich_text import CellRichText, TextBlock
from openpyxl.cell.text import InlineFont
from openpyxl.utils.datetime import CALENDAR_MAC_1904

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "tests", "fixtures", "edge")
FIXED = (1980, 1, 1, 0, 0, 0)


def openpyxl_types():
    wb = Workbook()
    ws = wb.active
    ws.title = "Types"
    ws.append(["text", "number", "date", "datetime", "time", "duration", "percent", "bool", "formula"])
    ws.append(["héllo 🌈", 3.25, dt.date(2024, 2, 29), dt.datetime(2024, 2, 29, 13, 45, 30), dt.time(6, 30), dt.timedelta(hours=36, minutes=15), 0.125, True, "=B2*2"])
    ws["G2"].number_format = "0.0%"
    ws["F2"].number_format = "[h]:mm:ss"
    ws.append(["line one\nline two", -1e-7, dt.date(1900, 1, 1), dt.datetime(1999, 12, 31, 23, 59, 59), None, None, None, False, '=IF(H3,"yes","no")'])
    ws["A4"] = CellRichText([TextBlock(InlineFont(b=True), "bold"), " and plain"])
    ws.merge_cells("A6:C7")
    ws["A6"] = "merged"
    hidden = wb.create_sheet("Hidden")
    hidden["B2"] = "secret"
    hidden.sheet_state = "hidden"
    very = wb.create_sheet("Very")
    very.sheet_state = "veryHidden"
    very["A1"] = 1
    wb.save(os.path.join(OUT, "openpyxl-types.xlsx"))


def openpyxl_1904():
    wb = Workbook()
    wb.epoch = CALENDAR_MAC_1904
    ws = wb.active
    ws["A1"] = dt.date(1904, 1, 1)
    ws["A2"] = dt.date(2024, 2, 29)
    ws["A3"] = dt.datetime(2000, 1, 1, 12, 0)
    wb.save(os.path.join(OUT, "openpyxl-1904.xlsx"))


def write_zip(name, parts):
    with zipfile.ZipFile(os.path.join(OUT, name), "w", zipfile.ZIP_DEFLATED) as z:
        for path, text in parts:
            info = zipfile.ZipInfo(path, FIXED)
            info.compress_type = zipfile.ZIP_DEFLATED
            z.writestr(info, text.encode("utf-8"))


MAIN = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
REL = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
PKG = "http://schemas.openxmlformats.org/package/2006/relationships"
CT = "http://schemas.openxmlformats.org/package/2006/content-types"


def content_types(overrides):
    items = "".join(f'<Override PartName="{p}" ContentType="{t}"/>' for p, t in overrides)
    return f'<?xml version="1.0" encoding="UTF-8"?><Types xmlns="{CT}"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/>{items}</Types>'


def sdk_style():
    """Prefixed elements, inline strings, missing r, shared formulas, CRLF,
    phonetic runs, single quotes, a chart sheet."""
    workbook = (
        f'<?xml version="1.0" encoding="utf-8"?><x:workbook xmlns:r="{REL}" xmlns:x="{MAIN}">'
        '<x:sheets><x:sheet name="Data" sheetId="1" r:id="R1a2b"/><x:sheet name="Chart" sheetId="2" r:id="R9"/></x:sheets></x:workbook>'
    )
    rels = (
        f'<?xml version="1.0" encoding="utf-8"?><Relationships xmlns="{PKG}">'
        f'<Relationship Type="{REL}/worksheet" Target="/xl/worksheets/sheet%201.xml" Id="R1a2b"/>'
        f'<Relationship Type="{REL}/chartsheet" Target="/xl/chartsheets/sheet1.xml" Id="R9"/>'
        f'<Relationship Type="{REL}/sharedStrings" Target="/xl/sharedStrings.xml" Id="R2"/>'
        f'<Relationship Type="{REL}/styles" Target="/xl/styles.xml" Id="R3"/></Relationships>'
    )
    sst = (
        f'<?xml version="1.0" encoding="utf-8"?><x:sst xmlns:x="{MAIN}">'
        "<x:si><x:t>shared</x:t></x:si>"
        '<x:si><x:r><x:rPr><x:b/></x:rPr><x:t>rich</x:t></x:r><x:r><x:t xml:space="preserve"> text</x:t></x:r></x:si>'
        '<x:si><x:t>東京</x:t><x:rPh sb="0" eb="2"><x:t>トウキョウ</x:t></x:rPh></x:si>'
        "<x:si><x:t>a\r\nb</x:t></x:si>"
        "<x:si><x:t>_x0001_ and _x005F_x0041_</x:t></x:si>"
        "</x:sst>"
    )
    styles = (
        f'<?xml version="1.0" encoding="utf-8"?><x:styleSheet xmlns:x="{MAIN}">'
        '<x:numFmts><x:numFmt numFmtId="164" formatCode="dd/mm/yyyy"/></x:numFmts>'
        '<x:fonts><x:font/></x:fonts><x:fills><x:fill/><x:fill/></x:fills><x:borders><x:border/></x:borders>'
        '<x:cellXfs><x:xf numFmtId="0"/><x:xf numFmtId="164" applyNumberFormat="1"/><x:xf numFmtId="22"/></x:cellXfs></x:styleSheet>'
    )
    sheet = (
        f"<?xml version='1.0' encoding='utf-8'?><x:worksheet xmlns:x='{MAIN}'><x:sheetData>"
        "<x:row><x:c t='s'><x:v>0</x:v></x:c><x:c t='s'><x:v>1</x:v></x:c><x:c t='s'><x:v>2</x:v></x:c><x:c t='s'><x:v>3</x:v></x:c><x:c t='s'><x:v>4</x:v></x:c></x:row>"
        "<x:row>\n  <x:c t='inlineStr'>\n    <x:is><x:t>inline</x:t></x:is>\n  </x:c>\n  <x:c s='1'><x:v>45351</x:v></x:c>\n  <x:c s='2'><x:v>45351.5</x:v></x:c>\n</x:row>"
        "<x:row r='5'><x:c r='A5'><x:v>1</x:v></x:c><x:c r='B5'><x:f t='shared' ref='B5:B7' si='0'>A5*10</x:f><x:v>10</x:v></x:c></x:row>"
        "<x:row r='6'><x:c r='A6'><x:v>2</x:v></x:c><x:c r='B6'><x:f t='shared' si='0'/><x:v>20</x:v></x:c></x:row>"
        "<x:row r='7'><x:c r='A7'><x:v>3</x:v></x:c><x:c r='B7'><x:f t='shared' si='0'/><x:v>30</x:v></x:c><x:c r='C7' t='str'><x:f>_xlfn.CONCAT(\"a\",\"b\")</x:f><x:v>ab</x:v></x:c></x:row>"
        "</x:sheetData></x:worksheet>"
    )
    chart = f'<?xml version="1.0" encoding="utf-8"?><x:chartsheet xmlns:x="{MAIN}"><x:sheetViews><x:sheetView workbookViewId="0"/></x:sheetViews></x:chartsheet>'
    write_zip(
        "sdk-style.xlsx",
        [
            ("[Content_Types].xml", content_types([
                ("/xl/workbook.xml", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"),
                ("/xl/worksheets/sheet 1.xml", "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"),
                ("/xl/chartsheets/sheet1.xml", "application/vnd.openxmlformats-officedocument.spreadsheetml.chartsheet+xml"),
                ("/xl/sharedStrings.xml", "application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"),
                ("/xl/styles.xml", "application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"),
            ])),
            ("_rels/.rels", f'<?xml version="1.0" encoding="utf-8"?><Relationships xmlns="{PKG}"><Relationship Type="{REL}/officeDocument" Target="/xl/workbook.xml" Id="R0"/></Relationships>'),
            ("xl/workbook.xml", workbook),
            ("xl/_rels/workbook.xml.rels", rels),
            ("xl/sharedStrings.xml", sst),
            ("xl/styles.xml", styles),
            ("xl/worksheets/sheet 1.xml", sheet),
            ("xl/chartsheets/sheet1.xml", chart),
        ],
    )


def strict():
    """ISO/IEC 29500 Strict: different namespaces and relationship types, and
    a main part that is not at xl/workbook.xml."""
    s_main = "http://purl.oclc.org/ooxml/spreadsheetml/main"
    s_rel = "http://purl.oclc.org/ooxml/officeDocument/relationships"
    write_zip(
        "strict.xlsx",
        [
            ("[Content_Types].xml", content_types([
                ("/book/main.xml", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"),
                ("/book/sheets/one.xml", "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"),
            ])),
            ("_rels/.rels", f'<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="{PKG}"><Relationship Id="rId1" Type="{s_rel}/officeDocument" Target="book/main.xml"/></Relationships>'),
            ("book/main.xml", f'<?xml version="1.0" encoding="UTF-8"?><workbook xmlns="{s_main}" xmlns:r="{s_rel}" conformance="strict"><sheets><sheet name="Strict" sheetId="1" r:id="rId1"/></sheets></workbook>'),
            ("book/_rels/main.xml.rels", f'<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="{PKG}"><Relationship Id="rId1" Type="{s_rel}/worksheet" Target="sheets/one.xml"/></Relationships>'),
            ("book/sheets/one.xml", f'<?xml version="1.0" encoding="UTF-8"?><worksheet xmlns="{s_main}"><sheetData><row r="1"><c r="A1" t="inlineStr"><is><t>strict</t></is></c><c r="B1"><v>1.5</v></c><c r="C1" t="b"><v>0</v></c></row></sheetData></worksheet>'),
        ],
    )


if __name__ == "__main__":
    os.makedirs(OUT, exist_ok=True)
    openpyxl_types()
    openpyxl_1904()
    sdk_style()
    strict()
    print("wrote", sorted(os.listdir(OUT)))
