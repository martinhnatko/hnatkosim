module Ram.Types.Slot exposing (..)
import Array exposing (Array)
import Html exposing (output)

type alias Slot =
    { name : String
    , inputText : String
    , inputTape : Array Int
    }
