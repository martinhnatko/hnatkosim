module Types.Model exposing (..)
import Dict exposing (Dict)
import Array exposing (Array)
import Types.ConsoleMessage exposing (ConsoleMessage)
import Types.Instructions exposing (Instruction)

-- MODEL
type alias Model =
    { inputText : String
    , registers : Dict Int Int
    , highlighted : Dict Int String
    , instructions : List Instruction
    , isRunning : Bool
    , simStarted : Bool
    , instructionPointer : Int
    , speeds : Array Int
    , speedIdx : Int
    , consoleMessages : List ConsoleMessage
    , slots : Array String
    , showSlotsModal : Bool
    }