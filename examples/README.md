# elm-xlsx example

A small app that:

1. writes a sample contact list (`Contacts.sample`) and downloads it,
2. opens any `.xlsx` file and lists its sheets,
3. shows the chosen sheet as a table,
4. decodes it into `Contact` records and reports the rows that don't fit,
5. exports the decoded records as a new file with `Xlsx.Encode.table`.

```sh
cd examples
elm make src/Main.elm --output=index.html
open index.html
```

`src/Readme.elm` holds the code samples from the package README, so that the compiler checks them.
