module Ram.Utils.HelperFunctions exposing (..)

import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Instructions exposing (Instruction(..))
import Ram.Types.Model exposing (Model)
import Shared.Types.ConsoleMessageType exposing (ConsoleMessageType)

import Json.Decode as Decode
import Json.Encode as Encode

import Time
import Task
import Dict
import Array exposing (Array)

requestAddMessage : (ConsoleMessageType, String) -> Cmd Msg
requestAddMessage (messageType, textOfMessage) =
    Time.now |> Task.perform (\posix -> AddMessageWithTime messageType posix textOfMessage)


getRegisterValue : Int -> Model -> Maybe Int
getRegisterValue regIndex model =
    Dict.get regIndex model.registers


encodeInputTape : Array Int -> String
encodeInputTape tape =
    Encode.encode 0 (Encode.list Encode.int (Array.toList tape))


decodeInputTape : String -> Maybe (Array Int)
decodeInputTape str =
    Decode.decodeString (Decode.array Decode.int) str
        |> Result.toMaybe