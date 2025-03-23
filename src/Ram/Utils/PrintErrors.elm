module Ram.Utils.PrintErrors exposing (printErrors)

import Ram.Utils.HelperFunctions exposing (requestAddMessage)

import Ram.Types.Instructions exposing (Instruction(..), Operand(..))
import Ram.Types.Messages exposing (Msg)
import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.ErrorType exposing (ErrorType(..))

import Shared.Components.Console exposing (ConsoleMessageType(..))

import List
import String

printErrors : List Instruction -> Cmd Msg
printErrors instructions =
    let
        unknownInstructionsMsg = checkForUnknownInstructions instructions

        nonExistingRegErrorMsg = checkForNonExistingRegisters instructions

        dbzMsg = checkForDividingByZero instructions

        duplicatedLabelsMsg = checkForDuplicatedLabels instructions

        referencingNonExistingLabelMsg = checkForReferencingNonExistingLabel instructions

        invalidInstructionsMsg = checkForInvalidInstructions instructions

        cmds = List.filterMap identity [ unknownInstructionsMsg, nonExistingRegErrorMsg, dbzMsg, duplicatedLabelsMsg, referencingNonExistingLabelMsg, invalidInstructionsMsg ]
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
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt unknownCount ++ " unknown instruction at position " ++ (String.join ", " (List.map String.fromInt unknownPositions)) ++ "." ))
    else if unknownCount > 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt unknownCount ++ " unknown instructions at positions: " ++ (String.join ", " (List.map String.fromInt unknownPositions)) ++ "." ))
    else
        Nothing


checkForNonExistingRegisters : List Instruction -> Maybe (Cmd Msg)
checkForNonExistingRegisters instructions =
    let
        nEREPositions : List Int
        nEREPositions =
            instructions
                |> List.indexedMap
                    (\i instr ->
                        case instr of
                            Load _ isError _ ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Store _ isError _ ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Add _ isError _ ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Sub _ isError _ ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Mul _ isError _ ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Div _ isError _ ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Read _ isError _ ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Write _ isError _ ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> List.filterMap identity

        nERECount = List.length nEREPositions

    in
    if nERECount == 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt nERECount ++ " instruction that is attempting to access a non-existing register at position " ++ (String.join ", " (List.map String.fromInt nEREPositions) ) ++ "."))
    else if nERECount > 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt nERECount ++ " instructions that are attempting to access a non-existing registers at positions: " ++ (String.join ", " (List.map String.fromInt nEREPositions) ) ++ "."))
    else
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
                            Div _ isError _ ->
                                case isError of
                                    Just DivByZero ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> List.filterMap identity

        dbzCount = List.length dbzPositions

    in
    if dbzCount == 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt dbzCount ++ " instruction that is dividing by zero at position " ++ (String.join ", " (List.map String.fromInt dbzPositions) ) ++ "."))
    else if dbzCount > 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt dbzCount ++ " instructions that are dividing by zero at positions: " ++ (String.join ", " (List.map String.fromInt dbzPositions) ) ++ "."))
    else
        Nothing


checkForDuplicatedLabels : List Instruction -> Maybe (Cmd Msg)
checkForDuplicatedLabels instructions =
    let
        duplicatedLabelsPositions =
            instructions
                |> List.indexedMap
                    (\i instr ->
                        case instr of
                            Label _ isError ->
                                case isError of
                                    Just DuplicatedLabel ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> List.filterMap identity

        duplicatedLabelsCount = List.length duplicatedLabelsPositions

    in
    if duplicatedLabelsCount == 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt duplicatedLabelsCount ++ " duplicated label at position: " ++ (String.join ", " (List.map String.fromInt duplicatedLabelsPositions)) ++ "."))
    else if duplicatedLabelsCount > 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt duplicatedLabelsCount ++ " duplicated labels at positions: " ++ (String.join ", " (List.map String.fromInt duplicatedLabelsPositions)) ++ "."))
    else
        Nothing


checkForReferencingNonExistingLabel : List Instruction -> Maybe (Cmd Msg)
checkForReferencingNonExistingLabel instructions =
    let
        nERLPositions : List Int
        nERLPositions =
            instructions
                |> List.indexedMap
                    (\i instr ->
                        case instr of
                            Jump _ _ isError _ ->
                                case isError of
                                    Just ReferencingNonExistingLabel ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Jzero _ _ isError _ ->
                                case isError of
                                    Just ReferencingNonExistingLabel ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Jgtz _ _ isError _ ->
                                case isError of
                                    Just ReferencingNonExistingLabel ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> List.filterMap identity

        nERLCount = List.length nERLPositions

    in
    if nERLCount == 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt nERLCount ++ " instruction that is referencing non-existing label at position " ++ (String.join ", " (List.map String.fromInt nERLPositions) ) ++ "."))
    else if nERLCount > 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt nERLCount ++ " instructions that are referencing non-existing labels at positions: " ++ (String.join ", " (List.map String.fromInt nERLPositions) ) ++ "."))
    else
        Nothing

checkForInvalidInstructions : List Instruction -> Maybe (Cmd Msg)
checkForInvalidInstructions instructions =
    let
        invalidPositions : List Int
        invalidPositions =
            instructions
                |> List.indexedMap
                    (\i instr ->
                        case instr of
                            Store _ isError _ ->
                                case isError of
                                    Just InvalidInstruction ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Read _ isError _ ->
                                case isError of
                                    Just InvalidInstruction ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Write _ isError _ ->
                                case isError of
                                    Just InvalidInstruction ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> List.filterMap identity

        invalidCount = List.length invalidPositions
    in 
    if invalidCount == 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt invalidCount ++ " invalid instruction at position " ++ (String.join ", " (List.map String.fromInt invalidPositions) ) ++ "."))
    else if invalidCount > 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt invalidCount ++ " invalid instructions at positions: " ++ (String.join ", " (List.map String.fromInt invalidPositions) ) ++ "."))
    else
        Nothing