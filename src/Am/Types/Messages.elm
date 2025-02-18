module Am.Types.Messages exposing (..)
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
    -- | RequestAddMessage String
    | AddMessageWithTime Time.Posix String
    | DeleteInput
    | SaveSlot Int
    | LoadSlot Int
    | ToggleSlotsModal
    | DeleteSlot Int