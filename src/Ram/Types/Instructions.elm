module Ram.Types.Instructions exposing (..)

import Ram.Types.ErrorType exposing (ErrorType)

type Instruction
    = Load Operand (Maybe ErrorType) Int
    | Store Operand (Maybe ErrorType) Int
    | Add Operand (Maybe ErrorType) Int
    | Sub Operand (Maybe ErrorType) Int
    | Mul Operand (Maybe ErrorType) Int
    | Div Operand (Maybe ErrorType) Int
    | Read Operand (Maybe ErrorType) Int
    | Write Operand (Maybe ErrorType) Int
    | Jump Int String (Maybe ErrorType) Int
    | Jzero Int String (Maybe ErrorType) Int
    | Jgtz Int String (Maybe ErrorType) Int
    | Halt Int
    | Label String (Maybe ErrorType)
    | UnknownInstruction

type Operand
    = Constant Int
    | Direct Int
    | Indirect Int