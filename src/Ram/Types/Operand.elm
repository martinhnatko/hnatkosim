module Ram.Types.Operand exposing (..)

type Operand
    = Constant Int
    | Direct Int
    | Indirect Int