module Am.Utils.PrintErrors exposing (..)

import Am.Types.ErrorType exposing (ErrorType(..))
import Am.Types.Instructions exposing (Instruction(..))
import Am.Types.Messages exposing (Msg(..))

import Am.Utils.HelperFunctions exposing (requestAddMessage)

import Shared.Types.ConsoleMessage exposing (ConsoleMessageType(..))

printErrors : List Instruction -> Cmd Msg
printErrors instructions =
    let
        unknownInstructionsMsg = checkForUnknownInstructions instructions

        nonExistingRegErrorMsg = checkForNonExistingRegisters instructions

        checkForUnmatchedStartLoopMsg = checkForUnmatchedStartLoop instructions

        checkForUnmatchedEndLoopMsg = checkForUnmatchedEndLoop instructions 

        cmds = List.filterMap identity [ unknownInstructionsMsg, nonExistingRegErrorMsg, checkForUnmatchedStartLoopMsg, checkForUnmatchedEndLoopMsg ]
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
                            Increment _ isError ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing
                            
                            Decrement _ isError ->
                                case isError of
                                    Just ReferencingNonExistingReg ->
                                        Just (i + 1)
                                    _ ->
                                        Nothing
                            
                            EndLoop _ _ isError ->
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


checkForUnmatchedStartLoop : List Instruction -> Maybe (Cmd Msg)
checkForUnmatchedStartLoop instructions =
    let
        dbzPositions : List Int
        dbzPositions =
            instructions
                |> List.indexedMap
                    (\i instr ->
                        case instr of
                            StartLoop _ _ isError ->
                                case isError of
                                    Just UnmatchedStartLoop ->
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
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt dbzCount ++ " unmatched start of the loop at position " ++ (String.join ", " (List.map String.fromInt dbzPositions) ) ++ "."))
    else if dbzCount > 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt dbzCount ++ " unmatched starts of the loops at positions: " ++ (String.join ", " (List.map String.fromInt dbzPositions) ) ++ "."))
    else
        Nothing

checkForUnmatchedEndLoop : List Instruction -> Maybe (Cmd Msg)
checkForUnmatchedEndLoop instructions =
    let
        dbzPositions : List Int
        dbzPositions =
            instructions
                |> List.indexedMap
                    (\i instr ->
                        case instr of
                            EndLoop _ _ isError ->
                                case isError of
                                    Just UnmatchedEndLoop ->
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
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt dbzCount ++ " unmatched end of the loop at position " ++ (String.join ", " (List.map String.fromInt dbzPositions) ) ++ "."))
    else if dbzCount > 1 then
        Just (requestAddMessage (ErrorMessage, "Parsing Error: Found " ++ String.fromInt dbzCount ++ " unmatched ends of the loops at positions: " ++ (String.join ", " (List.map String.fromInt dbzPositions) ) ++ "."))
    else
        Nothing