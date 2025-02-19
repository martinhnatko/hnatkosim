module Ram.Utils.PrintErrors exposing (printErrors)

import Shared.Types.ConsoleMessageType exposing (ConsoleMessageType(..))
import Ram.Types.Instructions exposing (Instruction(..))
import Ram.Types.Messages exposing (Msg)
import Ram.Utils.HelperFunctions exposing (requestAddMessage)
import Ram.Types.Operand exposing (Operand(..))
import Ram.Types.Messages exposing (Msg(..))

import List
import String
import Ram.Types.ErrorType exposing (ErrorType(..))

printErrors : List Instruction -> Cmd Msg
printErrors instructions =
    let
        unknownInstructionsMsg = checkForUnknownInstructions instructions

        nonExistingRegErrorMsg = checkForNonExistingRegisters instructions

        dbzMsg = checkForDividingByZero instructions

        duplicatedLabelsMsg = checkForDuplicatedLabels instructions

        referencingNonExistingLabelMsg = checkForReferencingNonExistingLabel instructions

        cmds = List.filterMap identity [ unknownInstructionsMsg, nonExistingRegErrorMsg, dbzMsg, duplicatedLabelsMsg, referencingNonExistingLabelMsg ]
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
                            Load _ isError ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Store _ isError ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Add _ isError ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Sub _ isError ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Mul _ isError ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Div _ isError ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Read _ isError ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Write _ isError ->
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
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt nERECount ++ " instruction that is referencing non-existing register at position " ++ (String.join ", " (List.map String.fromInt nEREPositions) ) ++ "."))
    else if nERECount > 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt nERECount ++ " instructions that are referencing non-existing registers at positions: " ++ (String.join ", " (List.map String.fromInt nEREPositions) ) ++ "."))
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
                            Div _ isError ->
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
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt duplicatedLabelsCount ++ " duplicated label: " ++ (String.join ", " (List.map String.fromInt duplicatedLabelsPositions)) ++ "."))
    else if duplicatedLabelsCount > 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt duplicatedLabelsCount ++ " duplicated labels: " ++ (String.join ", " (List.map String.fromInt duplicatedLabelsPositions)) ++ "."))
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
                            Jump _ _ isError ->
                                case isError of
                                    Just ReferencingNonExistingLabel ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Jzero _ _ isError ->
                                case isError of
                                    Just ReferencingNonExistingLabel ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing

                            Jgtz _ _ isError ->
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