port module Shared.Ports exposing (..)

-- PORTS
port scrollToBottom : String -> Cmd msg
port scrollInstructionIntoView : ( String, Int ) -> Cmd msg

port setItem : ( String, String ) -> Cmd msg
port getItem : String -> Cmd msg
port gotItem : ( (String, Maybe String) -> msg ) -> Sub msg

port subToTextArea : () -> Cmd msg

port requestMathJaxTypeset : () -> Cmd msg