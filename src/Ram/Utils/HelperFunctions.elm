module Ram.Utils.HelperFunctions exposing (..)

import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Instructions exposing (Instruction(..))

import Shared.Types.ConsoleMessage exposing (ConsoleMessageType)

import Json.Decode as Decode
import Json.Encode as Encode

import Time
import Task
import Array exposing (Array)

requestAddMessage : (ConsoleMessageType, String) -> Cmd Msg
requestAddMessage (messageType, textOfMessage) =
    Time.now |> Task.perform (\posix -> AddMessageWithTime messageType posix textOfMessage)

encodeInputTape : Array Int -> String
encodeInputTape tape =
    Encode.encode 0 (Encode.list Encode.int (Array.toList tape))


decodeInputTape : String -> Maybe (Array Int)
decodeInputTape str =
    Decode.decodeString (Decode.array Decode.int) str
        |> Result.toMaybe