module Ram.Utils.RamParser exposing (..)

import Char exposing (isDigit)
import List exposing (drop)
import Maybe

import Ram.Types.Instructions exposing (Instruction(..))
import Ram.Types.LabelDict exposing (LabelDict)
import Ram.Types.Operand exposing (Operand(..))

import Maybe
import Dict exposing (Dict)
import Svg.Attributes exposing (in_)

parseRAM : String -> (List Instruction, LabelDict)
parseRAM input =
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
        labels = Debug.log "labels" (findLabels nonEmptyLines)

        -- Parse each non-empty line into an instruction.
        instructions =
            List.map (\line -> parseInstruction labels line) nonEmptyLines
    in
    (instructions, labels)


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



parseInstruction : LabelDict -> String -> Instruction
parseInstruction labels line =
    let
        parts = List.filter (not << String.isEmpty) (String.split " " (String.trim line))
    in
    case parts of
        [ something ] ->
            case String.toUpper something of
                "HALT" ->
                    Halt

                _ ->            
                    if String.endsWith ":" something then
                        if String.length something > 1 then
                            Label (String.dropRight 1 something)
                        else
                            --Error
                            UnknownInstruction
                    else
                        --Error
                        UnknownInstruction

        instruction :: rest ->
            case String.toUpper instruction of
                "JUMP" ->
                    resolveJump labels (String.join " " rest) "Jump"

                "JZERO" ->
                    resolveJump labels (String.join " " rest) "Jzero"

                "JGTZ" ->
                    resolveJump labels (String.join " " rest) "Jgtz"

                other ->
                    let
                        operand = parseOperand (String.join " " (List.drop 1 parts))
                    in
                    if operand == Direct -1 then
                        -- ERROR
                        UnknownInstruction
                    else
                        case String.toUpper other of
                            "LOAD" ->
                                Load operand

                            "STORE" ->
                                Store operand

                            "ADD" ->
                                Add operand

                            "SUB" ->
                                Sub operand

                            "MUL" ->
                                Mul operand

                            "DIV" ->
                                Div operand

                            "READ" ->
                                Read operand

                            "WRITE" ->
                                Write operand

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
                "Jump" -> Jump address labelName
                "Jzero" -> Jzero address labelName
                "Jgtz" -> Jgtz address labelName
                _ -> UnknownInstruction

        Nothing ->
            --Error
            UnknownInstruction