module Ram.Types.ErrorType exposing (..)

type ErrorType
    = DivByZero
    | ReferencingNonExistingReg
    | DuplicatedLabel
    | ReferencingNonExistingLabel
    | InvalidInstruction
    -- | UnknownInstructionError

    -- | ReadingFromEmptyInputTape