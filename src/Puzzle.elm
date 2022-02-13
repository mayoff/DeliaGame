module Puzzle exposing (Date, Puzzle, dateFromIsoDateTime)


type alias Date =
    -- A date in the format YYYY-MM-DD
    String


dateFromIsoDateTime : String -> Date
dateFromIsoDateTime string =
    if
        (string |> String.left 4 |> String.all Char.isDigit)
            && (string |> String.dropLeft 4 |> String.startsWith "-")
            && (string |> String.dropLeft 5 |> String.left 2 |> String.all Char.isDigit)
            && (string |> String.dropLeft 7 |> String.startsWith "-")
            && (string |> String.dropLeft 8 |> String.left 2 |> String.all Char.isDigit)
            && (string |> String.dropLeft 10 |> String.startsWith "T")
    then
        String.left 10 string

    else
        "0000-00-00"


type alias Puzzle =
    { author : String
    , date : Date
    , hash : String
    , text : String
    }
