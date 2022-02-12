module Puzzle exposing (Date, Puzzle)


type alias Date =
    -- A date in the format YYYY-MM-DD
    String


type alias Puzzle =
    { author : String
    , date : Date
    , hash : String
    , text : String
    }
