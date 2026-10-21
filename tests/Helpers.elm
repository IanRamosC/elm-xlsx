module Helpers exposing (contains, containsAll, part, partNames, writeOk)

{-| Test helpers: write a workbook and look inside the resulting package.
-}

import Bytes exposing (Bytes)
import Expect exposing (Expectation)
import Xlsx.Write as Write
import Zip
import Zip.Entry


{-| Write with default options, failing the test on problems.
-}
writeOk : Write.Workbook -> Result String Bytes
writeOk wb =
    Write.toBytes Write.defaultOptions wb
        |> Result.mapError (List.map Write.problemToString >> String.join "\n")


{-| The paths inside the package, in order.
-}
partNames : Bytes -> List String
partNames bytes =
    Zip.fromBytes bytes
        |> Maybe.map (Zip.entries >> List.map Zip.Entry.path)
        |> Maybe.withDefault []


{-| The text of one part.
-}
part : String -> Bytes -> Maybe String
part path bytes =
    Zip.fromBytes bytes
        |> Maybe.andThen (Zip.getEntry path)
        |> Maybe.andThen (Zip.Entry.toString >> Result.toMaybe)


{-| Expect a part to contain a snippet.
-}
contains : String -> String -> Write.Workbook -> Expectation
contains path snippet wb =
    containsAll path [ snippet ] wb


{-| Expect a part to contain every snippet.
-}
containsAll : String -> List String -> Write.Workbook -> Expectation
containsAll path snippets wb =
    case writeOk wb of
        Err problems ->
            Expect.fail ("Expected the workbook to be written, got problems:\n" ++ problems)

        Ok bytes ->
            case part path bytes of
                Nothing ->
                    Expect.fail ("No part " ++ path ++ " in " ++ String.join ", " (partNames bytes))

                Just xml ->
                    case List.filter (\s -> not (String.contains s xml)) snippets of
                        [] ->
                            Expect.pass

                        missing ->
                            Expect.fail ("Missing from " ++ path ++ ":\n" ++ String.join "\n" missing ++ "\n\nPart:\n" ++ xml)
