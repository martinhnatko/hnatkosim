module Am.Utils.HelperFunctions exposing (..)

import Am.Types.Messages exposing (Msg(..))
import Am.Types.Instructions exposing (Instruction(..))

import Shared.Types.ConsoleMessage exposing (ConsoleMessageType)

import Time
import Task

requestAddMessage : (ConsoleMessageType, String) -> Cmd Msg
requestAddMessage (messageType, textOfMessage) =
    Time.now |> Task.perform (\posix -> AddMessageWithTime messageType posix textOfMessage)