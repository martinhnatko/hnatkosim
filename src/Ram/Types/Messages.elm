module Ram.Types.Messages exposing (..)
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
    | RemoveHighlightFromRegisters Int
    | RemoveHighlightFromInputTape Int
    | RemoveHighlightFromOutputTape Int
    | SwitchHighlight (Int, Int) (Int, Int, String)
    | RequestAddMessage String  -- Ask for a new console message with the current time
    | AddMessageWithTime Time.Posix String  -- Add a new console message with a given time
    | DeleteInput
    | SaveSlot Int
    | LoadSlot Int
    | ToggleSlotsModal
    | DeleteSlot Int
    | UpdateInputTape Int Int
    | AddCellToInputTape
    | RemoveLastCell
    | NoOp
