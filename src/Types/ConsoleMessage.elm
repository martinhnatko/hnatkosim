module Types.ConsoleMessage exposing (..)
import Time

type alias ConsoleMessage =
    { timestamp : Time.Posix
    , text : String
    }