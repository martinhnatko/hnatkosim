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
    , highlighted_registers : Dict Int String
    , highlighted_input_tape : Dict Int String
    , highlighted_output_tape : Dict Int String
    , instructions : List Instruction
    , isRunning : Bool
    , simStarted : Bool
    , instructionPointer : Int
    , speeds : Array Int
    , speedIdx : Int
    , consoleMessages : List ConsoleMessage
    , slots : Array String
    , slots_input_tapes : Array (Array Int)
    , showSlotsModal : Bool
    , labels : LabelDict
    , halted : Bool
    , inputTape : Array Int
    , inputTapePointer : Int
    , outputTape : Array Int
    }