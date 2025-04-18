module Ram.Types.Model exposing (..)

import Dict exposing (Dict)
import Array exposing (Array)
import Time exposing (Posix)

import Ram.Types.Instructions exposing (Instruction)
import Ram.Types.Slot exposing (Slot)

import Shared.Components.Console exposing (ConsoleMessage)

-- MODEL
type alias Model =
    { inputText : String
    , registers : Dict Int (Int, Maybe Int)
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
    , slots : Array Slot
    , halted : Bool
    , inputTape : Array Int
    , inputTapePointer : Int
    , outputTape : Array Int
    , showSlotsModal : Bool
    , showGuideModal : Bool
    , showSettingsModal : Bool
    , simulationStartTime : Maybe Posix
    , executedInstructions : Int
    , totalNumberOfRegisters : Int
    , totalMaxExecutedInstructions : Int
    , typedTotalNumberOfRegisters : String
    , typedTotalMaxExecutedInstructions : String
    , typedBase : String
    , logSpace : Int
    , logTime : Int
    , logBase : Int
    , tooManyRuntimeMsgs : Bool
    }