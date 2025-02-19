module Am.Types.ErrorType exposing (..)

type ErrorType
    = ReferencingNonExistingReg
    | UnmatchedStartLoop
    | UnmatchedEndLoop
    -- | UnknownInstructionError