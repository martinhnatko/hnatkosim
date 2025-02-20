module Shared.Types.ConsoleMessage exposing (..)

import Time

type alias ConsoleMessage =
    { messageType : ConsoleMessageType
    , timestamp : Time.Posix
    , text : String
    }

type ConsoleMessageType
    = InfoMessage
    | ErrorMessage
    | SimStarted
    | SimStopped