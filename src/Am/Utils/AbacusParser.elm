module Am.Utils.AbacusParser exposing (..)

import Am.Types.Instructions exposing (Instruction(..))
import Am.Types.ErrorType exposing (ErrorType(..))
import Am.Types.Model exposing (Model)

import Char exposing (isDigit)
import List exposing (drop)
import Maybe
import Dict


type alias LoopStack =
    List Int -- StartLoop index

parseAM : String -> Model -> List Instruction
parseAM input model =
    let
        inputToList = String.toList input

        instructions = parseInstructions inputToList [] [] 0 False model
    in
    instructions


parseInstructions : List Char -> List Instruction -> LoopStack -> Int -> Bool -> Model -> List Instruction
parseInstructions input instructions stack currentIndex parsingComment model =
    if parsingComment then
        case input of
            [] ->
                instructions

            '\n' :: rest ->
                parseInstructions rest instructions stack currentIndex False model

            _ :: rest ->
                parseInstructions rest instructions stack currentIndex True model

    else
        case input of
            [] ->
                -- If stack is non-empty, map all unmatched StartLoops to UnknownInstruction
                if List.isEmpty stack then
                    instructions
                else
                    instructions
                        |> List.map (\instr ->
                            case instr of
                                StartLoop endLoopIndex conditionIndex _ ->
                                    if endLoopIndex == -1 && conditionIndex == -1 then
                                        StartLoop currentIndex currentIndex (Just UnmatchedStartLoop)
                                    else
                                        instr
                                other ->
                                    other
                        )

            'a' :: rest ->
                let
                    digits = getFirstDigits rest
                    remaining = drop (List.length digits) rest
                    value = Maybe.withDefault 0 (String.fromList digits |> String.toInt)
                    -- newInstruction = Increment value
                in
                if Dict.member value model.registers then
                    parseInstructions remaining (instructions ++ [Increment value Nothing]) stack (currentIndex + 1) False model
                else
                    parseInstructions remaining (instructions ++ [Increment value (Just ReferencingNonExistingReg)]) stack (currentIndex + 1) False model
                
                -- parseInstructions remaining (instructions ++ [newInstruction]) stack (currentIndex + 1) False

            's' :: rest ->
                let
                    digits = getFirstDigits rest
                    remaining = drop (List.length digits) rest
                    value = Maybe.withDefault 0 (String.fromList digits |> String.toInt)
                in
                if Dict.member value model.registers then
                    parseInstructions remaining (instructions ++ [Decrement value Nothing]) stack (currentIndex + 1) False model
                else
                    parseInstructions remaining (instructions ++ [Decrement value (Just ReferencingNonExistingReg)]) stack (currentIndex + 1) False model

            '(' :: rest ->
                parseInstructions rest (instructions ++ [StartLoop -1 -1 Nothing]) (currentIndex :: stack) (currentIndex + 1) False model

            ')' :: rest ->
                let
                    digits = getFirstDigits rest
                    remaining = drop (List.length digits) rest
                    conditionIndex = Maybe.withDefault 0 (String.fromList digits |> String.toInt)
                in
                if not (Dict.member conditionIndex model.registers) then
                    parseInstructions remaining (instructions ++ [EndLoop -1 conditionIndex (Just ReferencingNonExistingReg)]) stack (currentIndex + 1) False model
                else
                    case stack of
                        [] ->
                            parseInstructions remaining (instructions ++ [ EndLoop -1 conditionIndex (Just UnmatchedEndLoop) ]) stack (currentIndex + 1) False model

                        startLoopIndex :: remainingStack ->
                            let
                                updatedInstructions =
                                    List.indexedMap
                                        (\i instr ->
                                            case instr of
                                                StartLoop _ _ _ ->
                                                    if i == startLoopIndex then
                                                        StartLoop currentIndex conditionIndex Nothing
                                                    else
                                                        instr

                                                _ ->
                                                    instr
                                        )
                                        instructions
                            in
                            parseInstructions remaining (updatedInstructions ++ [EndLoop startLoopIndex conditionIndex Nothing]) remainingStack (currentIndex + 1) False model

            '#' :: rest ->
                parseInstructions rest instructions stack currentIndex True model

            c :: rest ->
                if isWhitespace c then
                    parseInstructions rest instructions stack currentIndex False model
                else
                    parseInstructions rest (instructions ++ [UnknownInstruction]) stack (currentIndex + 1) False model


-- HELPER FUNCTION

getFirstDigits : List Char -> List Char
getFirstDigits chars =
    case chars of
        [] ->
            []

        c :: rest ->
            if isDigit c then
                c :: getFirstDigits rest
            else
                []


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t' || c == '\n' || c == '\r'