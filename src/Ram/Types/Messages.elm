module Ram.Types.Messages exposing (..)

import Time exposing (Posix)
import File exposing (File)

import Shared.Components.Console exposing (ConsoleMessageType)

-- MESSAGES
type Msg
    = UpdateCode String
    | Start
    | Tick Posix
    | Pause
    | Reset
    | Step
    | ChangeSpeed Int
    | RemoveHighlightFromRegisters Int
    | RemoveHighlightFromInputTape Int
    | RemoveHighlightFromOutputTape Int
    | SwitchHighlight (Int, Int) (Int, Int, String)
    | AddMessageWithTime ConsoleMessageType Posix String
    | DeleteInput
    | SaveSlot Int
    | LoadSlot Int
    | ToggleSlotsModal
    | ToggleGuideModal
    | ToggleSettingsModal
    | DeleteSlot Int
    | UpdateInputTape Int Int
    | AddCellToInputTape
    | RemoveLastCell
    | GoBackToMenu
    | UpdateSlotName Int String
    | NoOp
    | SetStartTime Posix
    | ComputeAndPrintDuration Bool Posix
    | StartInstantSimulation Posix
    | ChangeNumOfRegisters Int
    | ChangeMaxExecutedInstructions Int
    | TypedRegsNum String
    | TypedMaxExecutedInstructions String
    | TypedBase String
    | ChangeLogBase Int
    | RequestMathJaxUpdate
    | TriggerUpload Int
    | FileSelected Int File
    | FileLoaded Int String String
    | TriggerDownload Int
