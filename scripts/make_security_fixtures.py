"""Write hostile or broken files to tests/fixtures/security/.

Usage (from the repo root):
    python3 scripts/make_security_fixtures.py

Each file must be refused with a clear Xlsx.Read error, never crash, hang or
use unbounded memory. tests/SecurityTest.elm checks them.
"""

import io
import os
import zipfile

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "tests", "fixtures", "security")
FIXED = (1980, 1, 1, 0, 0, 0)

CT = '<?xml version="1.0" encoding="UTF-8"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/></Types>'
ROOT_RELS = '<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>'
WB = '<?xml version="1.0" encoding="UTF-8"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="S" sheetId="1" r:id="rId1"/></sheets></workbook>'
WB_RELS = '<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/></Relationships>'
SHEET = '<?xml version="1.0" encoding="UTF-8"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData><row r="1"><c r="A1"><v>1</v></c></row></sheetData></worksheet>'


def package(name, overrides=None, extra=None, method=zipfile.ZIP_DEFLATED):
    parts = {
        "[Content_Types].xml": CT,
        "_rels/.rels": ROOT_RELS,
        "xl/workbook.xml": WB,
        "xl/_rels/workbook.xml.rels": WB_RELS,
        "xl/worksheets/sheet1.xml": SHEET,
    }
    parts.update(overrides or {})
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", method) as z:
        for path, data in list(parts.items()) + list((extra or {}).items()):
            info = zipfile.ZipInfo(path, FIXED)
            info.compress_type = method
            z.writestr(info, data if isinstance(data, bytes) else data.encode("utf-8"))
    write(name, buf.getvalue())


def write(name, data):
    with open(os.path.join(OUT, name), "wb") as f:
        f.write(data)


def main():
    os.makedirs(OUT, exist_ok=True)

    # 1. Billion laughs: entity expansion through a DTD, in the worksheet and
    #    in the workbook. Refused because of the DOCTYPE, before parsing.
    laughs = '<?xml version="1.0"?><!DOCTYPE lolz [<!ENTITY lol "lol">' + "".join(
        f'<!ENTITY lol{i} "{("&lol" + (str(i - 1) if i > 1 else "") + ";") * 10}">' for i in range(1, 10)
    ) + "]>"
    package("billion-laughs-sheet.xlsx", {"xl/worksheets/sheet1.xml": laughs + '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData><row r="1"><c r="A1" t="inlineStr"><is><t>&lol9;</t></is></c></row></sheetData></worksheet>'})
    package("billion-laughs-workbook.xlsx", {"xl/workbook.xml": laughs + WB.split("?>", 1)[1]})

    # 2. External entity (XXE) reading a local file.
    package("xxe.xlsx", {"xl/sharedStrings.xml": '<?xml version="1.0"?><!DOCTYPE sst [<!ENTITY x SYSTEM "file:///etc/passwd">]><sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><si><t>&x;</t></si></sst>',
             "xl/_rels/workbook.xml.rels": WB_RELS.replace("</Relationships>", '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/></Relationships>')})

    # 3. Zip bomb: 32 MB of zeros that deflate to ~32 KB (ratio ~0.001).
    package("zip-bomb.xlsx", extra={"xl/media/bomb.bin": bytes(32 * 1024 * 1024)})

    # 4. Deeply nested XML in a part read with the DOM parser (stack depth).
    package("deep-nesting.xlsx", {"xl/workbook.xml": WB.replace("<sheets>", "<x>" * 100000 + "</x>" * 100000 + "<sheets>")})

    # 4b. Very many siblings: too big to embed; tests/StylesReaderTest.elm
    #     builds a 200k-entry styles.xml instead.

    # 5. Malformed XML in the workbook.
    package("malformed-workbook.xlsx", {"xl/workbook.xml": WB.replace("</sheets>", "</sheet>")})

    # 6. A truncated file: the first half of a valid one.
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", zipfile.ZIP_DEFLATED) as z:
        for path, data in {"[Content_Types].xml": CT, "_rels/.rels": ROOT_RELS, "xl/workbook.xml": WB, "xl/worksheets/sheet1.xml": SHEET * 20}.items():
            z.writestr(zipfile.ZipInfo(path, FIXED), data)
    full = buf.getvalue()
    write("truncated.xlsx", full[: len(full) // 2])

    # 7. A damaged entry: valid directory, flipped bytes in compressed data.
    package("bad-crc.xlsx", method=zipfile.ZIP_STORED)
    path = os.path.join(OUT, "bad-crc.xlsx")
    data = bytearray(open(path, "rb").read())
    i = data.find(b"<sheetData>")
    data[i + 1] ^= 0xFF
    write("bad-crc.xlsx", bytes(data))

    # 8. Legacy / encrypted: an OLE2 compound file header.
    write("encrypted.xlsx", bytes.fromhex("D0CF11E0A1B11AE1") + bytes(504))

    # 9. Binary workbook.
    package("binary.xlsb", extra={"xl/workbook.bin": b"\x83\x01\x00"})

    # 10. UTF-16 worksheet.
    package("utf16.xlsx", {"xl/worksheets/sheet1.xml": SHEET.replace('encoding="UTF-8"', 'encoding="UTF-16"').encode("utf-16")})

    # 11. Absurd coordinates: a cell at XFD1048576, and a row number past it.
    package("far-cell.xlsx", {"xl/worksheets/sheet1.xml": SHEET.replace('<row r="1"><c r="A1"><v>1</v></c></row>', '<row r="1048576"><c r="XFD1048576"><v>1</v></c></row><row r="99999999"><c r="ZZZZ99999999"><v>2</v></c></row>')})

    # 12. A relationship that escapes the package and one to a missing part.
    package("bad-targets.xlsx", {"xl/_rels/workbook.xml.rels": WB_RELS.replace("worksheets/sheet1.xml", "../../../../etc/passwd")})

    # 13. Empty file and a random non-zip file.
    write("empty.xlsx", b"")
    write("not-a-zip.xlsx", b"PK\x05\x06" + bytes(18))

    print("wrote", sorted(os.listdir(OUT)))


if __name__ == "__main__":
    main()
