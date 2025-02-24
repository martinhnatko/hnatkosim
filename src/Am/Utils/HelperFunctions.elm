module Am.Utils.HelperFunctions exposing (..)

import Am.Types.Messages exposing (Msg(..))
import Am.Types.Instructions exposing (Instruction(..))
import Am.Types.Slot exposing (Slot)

import Shared.Types.ConsoleMessage exposing (ConsoleMessageType)

import Time
import Task

import Json.Encode as Encode

requestAddMessage : (ConsoleMessageType, String) -> Cmd Msg
requestAddMessage (messageType, textOfMessage) =
    Time.now |> Task.perform (\posix -> AddMessageWithTime messageType posix textOfMessage)

encodeSlot : Slot -> String
encodeSlot slot =
    Encode.encode 0 <|
        Encode.object
            [ ( "name", Encode.string slot.name )
            , ( "inputText", Encode.string slot.inputText )
            ]