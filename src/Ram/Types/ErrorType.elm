module Ram.Types.ErrorType exposing (..)

type ErrorType
    = DivByZero
    | ReferencingNonExistingReg
    | DuplicatedLabel
    | ReferencingNonExistingLabel
    -- | UnknownInstructionError

    -- | ReadingFromEmptyInputTape