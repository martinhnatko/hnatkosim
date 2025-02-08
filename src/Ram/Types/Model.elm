module Ram.Types.Model exposing (..)
import Dict exposing (Dict)
import Array exposing (Array)
import Ram.Types.ConsoleMessage exposing (ConsoleMessage)
import Ram.Types.Instructions exposing (Instruction)
import Ram.Types.LabelDict exposing (LabelDict)

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
    , labels : LabelDict
    , halted : Bool
    , inputTape : Array ( Maybe Int )
    , inputTapePointer : Int
    , outputTape : Array ( Maybe Int )
    , outputTapePointer : Int
    , maybeFocusedCell : Maybe Int
    , pendingFocus : Maybe Int
    }