module Am.Utils.HelperFunctions exposing (..)

import Am.Types.Messages exposing (Msg(..))
import Am.Types.Instructions exposing (Instruction(..))

import Shared.Types.ConsoleMessageType exposing (ConsoleMessageType)

import Time
import Task

requestAddMessage : (ConsoleMessageType, String) -> Cmd Msg
requestAddMessage (messageType, textOfMessage) =
    Time.now |> Task.perform (\posix -> AddMessageWithTime messageType posix textOfMessage)

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