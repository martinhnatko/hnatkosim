module Am.Utils.HelperFunctions exposing (..)
import Am.Types.Messages exposing (Msg(..))
import Am.Types.Instructions exposing (Instruction(..))
import Time
import Task

requestAddMessages : List String -> Cmd Msg
requestAddMessages msgs =
    msgs
        |> List.map (\msg -> Time.now |> Task.perform (\posix -> AddMessageWithTime posix msg))
        |> Cmd.batch





checkForErrors : List Instruction -> List String
checkForErrors instructions =
    let
        unknownInstructions = 
            instructions
                |> List.filter (\i -> i == UnknownInstruction)
                |> List.length
    in
    if unknownInstructions > 0 then
        [ "Error: Found " ++ String.fromInt unknownInstructions ++ " unknown instructions" ]
    else
        []