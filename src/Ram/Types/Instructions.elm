module Ram.Types.Instructions exposing (..)

import Ram.Types.Operand exposing (Operand)

type Instruction
    = Load Operand
    | Store Operand
    | Add Operand
    | Sub Operand
    | Mul Operand
    | Div Operand
    | Read Operand
    | Write Operand
    | Jump Int String
    | Jzero Int String
    | Jgtz Int String
    | Halt
    | Label String
    | UnknownInstruction