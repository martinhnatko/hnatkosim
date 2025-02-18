module Shared.Types.ConsoleMessage exposing (..)

import Shared.Types.ConsoleMessageType exposing (ConsoleMessageType)

import Time

type alias ConsoleMessage =
    { messageType : ConsoleMessageType
    , timestamp : Time.Posix
    , text : String
    }
