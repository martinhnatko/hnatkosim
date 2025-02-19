module Am.Types.Model exposing (..)

import Am.Types.Instructions exposing (Instruction)

import Shared.Types.ConsoleMessage exposing (ConsoleMessage)

import Dict exposing (Dict)
import Array exposing (Array)

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