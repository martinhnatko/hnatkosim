module Ram.Types.Messages exposing (..)

import Time

import Shared.Types.ConsoleMessage exposing (ConsoleMessageType)

-- MESSAGES
type Msg
    = UpdateCode String
    | Start
    | Tick Time.Posix
    | Pause
    | Reset
    | Step
    | ChangeSpeed Int
    | RemoveHighlightFromRegisters Int
    | RemoveHighlightFromInputTape Int
    | RemoveHighlightFromOutputTape Int
    | SwitchHighlight (Int, Int) (Int, Int, String)
    | AddMessageWithTime ConsoleMessageType Time.Posix String
    | DeleteInput
    | SaveSlot Int
    | LoadSlot Int
    | ToggleSlotsModal
    | DeleteSlot Int
    | UpdateInputTape Int Int
    | AddCellToInputTape
    | RemoveLastCell
    | GoBackToMenu
    | NoOp
