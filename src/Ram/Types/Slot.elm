module Ram.Types.Slot exposing (..)

import Array exposing (Array)

type alias Slot =
    { name : String
    , inputText : String
    , inputTape : Array Int
    }
