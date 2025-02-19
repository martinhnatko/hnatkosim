module Am.Types.Instructions exposing (..)

import Am.Types.ErrorType exposing (ErrorType)

type Instruction
    = Increment Int (Maybe ErrorType)
    | Decrement Int (Maybe ErrorType)
    | StartLoop Int Int (Maybe ErrorType)
    | EndLoop Int Int (Maybe ErrorType)
    | UnknownInstruction