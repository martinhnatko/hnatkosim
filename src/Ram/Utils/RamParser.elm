module Ram.Utils.RamParser exposing (..)

import Ram.Types.Instructions exposing (Instruction(..), Operand(..))
import Ram.Types.Model exposing (Model)
import Ram.Types.ErrorType exposing (ErrorType(..))

import Dict exposing (Dict)
import Maybe

type alias LabelDict = Dict String Int

parseRAM : String -> Model -> List Instruction
parseRAM input model =
    let
        -- Split the input into raw lines.
        rawLines =
            String.split "\n" input

        -- Remove comments: for each line, split by "#" and keep the first part.
        cleanedLines =
            List.map (String.split "#" >> List.head >> Maybe.withDefault "") rawLines

        -- Filter out lines that are empty after trimming.
        nonEmptyLines =
            List.filter (\line -> String.trim line /= "") cleanedLines

        -- Extract labels from non-empty lines.
        labels = findLabels nonEmptyLines

        -- Parse each non-empty line into an instruction.
        instructions =
            List.indexedMap (\idx line -> parseInstruction labels line idx model) nonEmptyLines
    in
    instructions


findLabels : List String -> LabelDict
findLabels lines =
    let
        indexedLines = List.indexedMap (\i line -> ( i, line )) lines

        updateLabelDict ( idx, line ) dict =
            let
                actualLine = String.toUpper (String.trim line)
            in
            if String.endsWith ":" actualLine then
                let
                    label = String.trim (String.dropRight 1 actualLine)
                in
                Dict.insert label idx dict
            else
                dict
    in
    -- Fold over all indexed lines, building up the dictionary.
    List.foldl updateLabelDict Dict.empty indexedLines



parseInstruction : LabelDict -> String -> Int -> Model -> Instruction
parseInstruction labels line idx model =
    let
        parts = List.filter (not << String.isEmpty) (String.split " " (String.trim line))
    in
    case parts of
        [ something ] ->
            case String.toUpper something of
                "HALT" ->
                    Halt 0

                _ ->            
                    if String.endsWith ":" something then
                        if String.length something > 1 then
                            if (Maybe.withDefault -1 (Dict.get (String.toUpper (String.dropRight 1 something)) labels) == idx) then
                                Label (String.dropRight 1 something) Nothing
                            else
                                Label (String.dropRight 1 something) (Just DuplicatedLabel) 
                        else
                            --Error
                            UnknownInstruction
                    else
                        --Error
                        UnknownInstruction

        instruction :: rest ->
            case String.toUpper instruction of
                "JUMP" ->
                    case rest of
                        [ label ] ->
                            resolveJump labels label "Jump"

                        _ ->
                            UnknownInstruction
                "JZERO" ->
                    case rest of
                        [ label ] ->
                            resolveJump labels label "Jzero"

                        _ ->
                            UnknownInstruction

                "JGTZ" ->
                    case rest of
                        [ label ] ->
                            resolveJump labels label "Jgtz"

                        _ ->
                            UnknownInstruction

                other ->
                    let
                        operand = parseOperand (String.join " " (List.drop 1 parts))
                    in
                    if operand == Direct -1 then
                        UnknownInstruction
                    else
                        case String.toUpper other of
                            "LOAD" ->
                                case operand of
                                    Constant _ -> 
                                        Load operand Nothing 0
                                    
                                    Direct n ->
                                        if Dict.member n model.registers then
                                            Load operand Nothing 0
                                        else
                                            Load operand (Just ReferencingNonExistingReg) 0

                                    Indirect n ->
                                        if Dict.member n model.registers then
                                            Load operand Nothing 0
                                        else
                                            Load operand (Just ReferencingNonExistingReg) 0

                            "STORE" ->
                                case operand of
                                    Constant _ -> 
                                        Store operand (Just InvalidInstruction) 0
                                    
                                    Direct n ->
                                        if Dict.member n model.registers then
                                            Store operand Nothing 0
                                        else
                                            Store operand (Just ReferencingNonExistingReg) 0

                                    Indirect n ->
                                        if Dict.member n model.registers then
                                            Store operand Nothing 0
                                        else
                                            Store operand (Just ReferencingNonExistingReg) 0

                            "ADD" ->
                                case operand of
                                    Constant _ -> 
                                        Add operand Nothing 0
                                    
                                    Direct n ->
                                        if Dict.member n model.registers then
                                            Add operand Nothing 0
                                        else
                                            Add operand (Just ReferencingNonExistingReg) 0

                                    Indirect n ->
                                        if Dict.member n model.registers then
                                            Add operand Nothing 0
                                        else
                                            Add operand (Just ReferencingNonExistingReg) 0
 
                            "SUB" ->
                                case operand of
                                    Constant _ -> 
                                        Sub operand Nothing 0
                                    
                                    Direct n ->
                                        if Dict.member n model.registers then
                                            Sub operand Nothing 0
                                        else
                                            Sub operand (Just ReferencingNonExistingReg) 0

                                    Indirect n ->
                                        if Dict.member n model.registers then
                                            Sub operand Nothing 0
                                        else
                                            Sub operand (Just ReferencingNonExistingReg) 0

                            "MUL" ->
                                case operand of
                                    Constant _ -> 
                                        Mul operand Nothing 0
                                    
                                    Direct n ->
                                        if Dict.member n model.registers then
                                            Mul operand Nothing 0
                                        else
                                            Mul operand (Just ReferencingNonExistingReg) 0

                                    Indirect n ->
                                        if Dict.member n model.registers then
                                            Mul operand Nothing 0
                                        else
                                            Mul operand (Just ReferencingNonExistingReg) 0

                            "DIV" ->
                                case operand of
                                    Constant value ->
                                        if value == 0 then
                                            Div operand (Just DivByZero) 0
                                        else
                                            Div operand Nothing  0
                                    
                                    Direct n ->
                                        if Dict.member n model.registers then
                                            Div operand Nothing 0
                                        else
                                            Div operand (Just ReferencingNonExistingReg) 0

                                    Indirect n ->
                                        if Dict.member n model.registers then
                                            Div operand Nothing 0
                                        else
                                            Div operand (Just ReferencingNonExistingReg) 0

                            "READ" ->
                                case operand of
                                    Constant _ -> 
                                        Read operand (Just InvalidInstruction) 0
                                    
                                    Direct n ->
                                        if Dict.member n model.registers then
                                            Read operand Nothing 0
                                        else
                                            Read operand (Just ReferencingNonExistingReg) 0

                                    Indirect n ->
                                        if Dict.member n model.registers then
                                            Read operand Nothing 0
                                        else
                                            Read operand (Just ReferencingNonExistingReg) 0

                            "WRITE" ->
                                case operand of
                                    Constant _ -> 
                                        Write operand (Just InvalidInstruction) 0
                                    
                                    Direct n ->
                                        if Dict.member n model.registers then
                                            Write operand Nothing 0
                                        else
                                            Write operand (Just ReferencingNonExistingReg) 0

                                    Indirect n ->
                                        if Dict.member n model.registers then
                                            Write operand Nothing 0
                                        else
                                            Write operand (Just ReferencingNonExistingReg) 0

                            _ ->
                                UnknownInstruction
        _ -> UnknownInstruction



parseOperand : String -> Operand
parseOperand input =
    let
        trimmedInput = String.trim input
    in
    if String.startsWith "=" trimmedInput then
        case String.toInt (String.dropLeft 1 trimmedInput) of
            Just n -> Constant n
            Nothing -> Direct -1  -- Error case
            
    else if String.startsWith "*" trimmedInput then
        case String.toInt (String.dropLeft 1 trimmedInput) of
            Just n -> Indirect n
            Nothing -> Direct -1  -- Error case
            
    else
        case String.toInt trimmedInput of
            Just n -> Direct n
            Nothing -> Direct -1  -- Error case

resolveJump : LabelDict -> String -> String -> Instruction
resolveJump labels labelName instruction =
    case Dict.get (String.toUpper labelName) labels of
        Just address ->
            case instruction of
                "Jump" -> Jump address labelName Nothing 0
                "Jzero" -> Jzero address labelName Nothing 0
                "Jgtz" -> Jgtz address labelName Nothing 0
                _ -> UnknownInstruction

        Nothing ->
            case instruction of
                "Jump" -> Jump 0 labelName (Just ReferencingNonExistingLabel) 0
                "Jzero" -> Jzero 0 labelName (Just ReferencingNonExistingLabel) 0
                "Jgtz" -> Jgtz 0 labelName (Just ReferencingNonExistingLabel) 0
                _ -> UnknownInstruction