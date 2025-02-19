module Am.Types.Messages exposing (..)

import Shared.Types.ConsoleMessageType exposing (ConsoleMessageType)

import Time

-- MESSAGES
type Msg
    = UpdateCode String
    | Start
    | Tick Time.Posix
    | Pause
    | Reset
    | Step
    | ChangeSpeed Int
    | RemoveHighlight Int
    | AddMessageWithTime ConsoleMessageType Time.Posix String
    | DeleteInput
    | SaveSlot Int
    | LoadSlot Int
    | ToggleSlotsModal
    | DeleteSlot Int