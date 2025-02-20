module Ram.Types.Instructions exposing (..)

import Ram.Types.ErrorType exposing (ErrorType)

type Instruction
    = Load Operand (Maybe ErrorType)
    | Store Operand (Maybe ErrorType)
    | Add Operand (Maybe ErrorType)
    | Sub Operand (Maybe ErrorType)
    | Mul Operand (Maybe ErrorType)
    | Div Operand (Maybe ErrorType)
    | Read Operand (Maybe ErrorType)
    | Write Operand (Maybe ErrorType)
    | Jump Int String (Maybe ErrorType)
    | Jzero Int String (Maybe ErrorType)
    | Jgtz Int String (Maybe ErrorType)
    | Halt
    | Label String (Maybe ErrorType)
    | UnknownInstruction

type Operand
    = Constant Int
    | Direct Int
    | Indirect Int