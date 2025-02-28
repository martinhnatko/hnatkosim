module Am.Types.Messages exposing (..)

import Shared.Types.ConsoleMessage exposing (ConsoleMessageType)

import Time exposing (Posix)

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
    | DeleteSlot Int
    | GoBackToMenu
    | UpdateSlotName Int String
    | SetStartTime Posix
    | ComputeAndPrintDuration Posix
    | StartInstantSimulation Posix