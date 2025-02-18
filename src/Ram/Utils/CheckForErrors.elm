module Ram.Utils.CheckForErrors exposing (checkForErrors)

import Shared.Types.ConsoleMessageType exposing (ConsoleMessageType(..))
import Ram.Types.Instructions exposing (Instruction(..))
import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg)
import Ram.Utils.HelperFunctions exposing (requestAddMessage)
import Ram.Types.Operand exposing (Operand(..))
import Ram.Types.Messages exposing (Msg(..))

import List
import String
import Dict

checkForErrors : List Instruction -> Model -> Cmd Msg
checkForErrors instructions model =
    let
        unknownInstructionsMsg = checkForUnknownInstructions instructions

        nonExistingRegErrorMsg = checkForNonExistingRegisters instructions model

        dbzMsg = checkForDividingByZero instructions

        cmds = List.filterMap identity [ unknownInstructionsMsg, nonExistingRegErrorMsg, dbzMsg ]
    in
    Cmd.batch cmds


checkForUnknownInstructions : List Instruction -> Maybe (Cmd Msg)
checkForUnknownInstructions instructions =
    let
        unknownPositions : List Int
        unknownPositions =
            instructions
                |> List.indexedMap (\i instr -> if instr == UnknownInstruction then Just (i + 1) else Nothing)
                |> List.filterMap identity

        unknownCount = List.length unknownPositions

    in
    if unknownCount == 1 then
        Just (requestAddMessage (ErrorMessage, "Error: Found " ++ String.fromInt unknownCount ++ " unknown instruction at position " ++ (String.join ", " (List.map String.fromInt unknownPositions)) ++ "." ))
    else if unknownCount > 1 then
        Just (requestAddMessage (ErrorMessage, "Error: Found " ++ String.fromInt unknownCount ++ " unknown instructions at positions: " ++ (String.join ", " (List.map String.fromInt unknownPositions)) ++ "." ))
    else
        Nothing


checkForNonExistingRegisters : List Instruction -> Model -> Maybe (Cmd Msg)
checkForNonExistingRegisters instructions model =
    let
        nEREPositions : List Int
        nEREPositions =
            instructions
                |> List.indexedMap
                    (\i instr ->
                        case instr of
                            Load operand ->
                                checkOperand i operand model

                            Store operand ->
                                checkOperand i operand model

                            Add operand ->
                                checkOperand i operand model

                            Sub operand ->
                                checkOperand i operand model

                            Mul operand ->
                                checkOperand i operand model

                            Div operand ->
                                checkOperand i operand model

                            Read operand ->
                                checkOperand i operand model

                            Write operand ->
                                checkOperand i operand model

                            _ ->
                                Nothing
                    )
                |> List.filterMap identity

        nERECount = List.length nEREPositions

    in
    if nERECount == 1 then
        Just (requestAddMessage (ErrorMessage, "Error: Found " ++ String.fromInt nERECount ++ " instruction that is referencing non-existing register at position " ++ (String.join ", " (List.map String.fromInt nEREPositions) ) ++ "."))
    else if nERECount > 1 then
        Just (requestAddMessage (ErrorMessage, "Error: Found " ++ String.fromInt nERECount ++ " instructions that are referencing non-existing registers at positions: " ++ (String.join ", " (List.map String.fromInt nEREPositions) ) ++ "."))
    else
        Nothing
    
checkOperand : Int -> Operand -> Model -> Maybe Int
checkOperand i operand model =
    case operand of
        Direct n ->
            if Dict.member n model.registers then
                Nothing
            else
                Just (i + 1)

        Indirect n ->
            if Dict.member n model.registers then
                Nothing
            else
                Just (i + 1)

        _ ->
            Nothing

checkForDividingByZero : List Instruction -> Maybe (Cmd Msg)
checkForDividingByZero instructions =
    let
        dbzPositions : List Int
        dbzPositions =
            instructions
                |> List.indexedMap
                    (\i instr ->
                        case instr of
                            Div operand ->
                                case operand of
                                    Constant n ->
                                        if n == 0 then
                                            Just (i + 1)
                                        else
                                            Nothing

                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> List.filterMap identity

        dbzCount = List.length dbzPositions

    in
    if dbzCount == 1 then
        Just (requestAddMessage (ErrorMessage, "Error: Found " ++ String.fromInt dbzCount ++ " instruction that is dividing by zero at position " ++ (String.join ", " (List.map String.fromInt dbzPositions) ) ++ "."))
    else if dbzCount > 1 then
        Just (requestAddMessage (ErrorMessage, "Error: Found " ++ String.fromInt dbzCount ++ " instructions that are dividing by zero at positions: " ++ (String.join ", " (List.map String.fromInt dbzPositions) ) ++ "."))
    else
        Nothing

