module Am.Types.Messages exposing (..)

import Shared.Components.Console exposing (ConsoleMessageType)

import Time exposing (Posix)
import File exposing (File)
import Ram.Types.Messages exposing (Msg(..))

-- MESSAGES
type Msg
    = UpdateCode String
    | Start
    | Tick Posix
    | Pause
    | Reset
    | Step
    | ChangeSpeed Int
    | RemoveHighlight Int
    | AddMessageWithTime ConsoleMessageType Posix String
    | DeleteInput
    | SaveSlot Int
    | LoadSlot Int
    | ToggleSlotsModal
    | ToggleGuideModal
    | ToggleSettingsModal
    | DeleteSlot Int
    | GoBackToMenu
    | UpdateSlotName Int String
    | SetStartTime Posix
    | ComputeAndPrintDuration Posix
    | StartInstantSimulation Posix
    | ChangeNumOfRegisters Int
    | ChangeMaxExecutedInstructions Int
    | TypedRegsNum String
    | TypedMaxExecutedInstructions String
    | NoOp
    | TriggerUpload Int
    | FileSelected Int File
    | FileLoaded Int String String
    | TriggerDownload Int