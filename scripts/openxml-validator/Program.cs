// Validate .xlsx files against the Office 2019 schema with the Open XML SDK.
// Usage (from the repo root): dotnet run --project scripts/openxml-validator -- scripts/out/*.xlsx
// Exits with status 1 if any file has errors.
using DocumentFormat.OpenXml;
using DocumentFormat.OpenXml.Packaging;
using DocumentFormat.OpenXml.Validation;

var failed = false;
var validator = new OpenXmlValidator(FileFormatVersions.Office2019);

foreach (var path in args)
{
    try
    {
        using var doc = SpreadsheetDocument.Open(path, false);
        var errors = validator.Validate(doc).ToList();
        Console.WriteLine($"{Path.GetFileName(path)}: {errors.Count} errors");
        foreach (var e in errors)
        {
            failed = true;
            Console.WriteLine($"  {e.Part?.Uri} {e.Path?.XPath}: {e.Description}");
        }
    }
    catch (Exception ex)
    {
        failed = true;
        Console.WriteLine($"{Path.GetFileName(path)}: cannot open: {ex.Message}");
    }
}

return failed ? 1 : 0;
