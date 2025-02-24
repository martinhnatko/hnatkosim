module Ram.Utils.HelperFunctions exposing (..)

import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Instructions exposing (Instruction(..))
import Ram.Types.Slot exposing (Slot)

import Shared.Types.ConsoleMessage exposing (ConsoleMessageType)

import Json.Encode as Encode

import Time
import Task
import Array

requestAddMessage : (ConsoleMessageType, String) -> Cmd Msg
requestAddMessage (messageType, textOfMessage) =
    Time.now |> Task.perform (\posix -> AddMessageWithTime messageType posix textOfMessage)

encodeSlot : Slot -> String
encodeSlot slot =
    Encode.encode 0 <|
        Encode.object
            [ ( "name", Encode.string slot.name )
            , ( "inputText", Encode.string slot.inputText )
            , ( "inputTape", (Encode.list Encode.int (Array.toList slot.inputTape)) )
            ]