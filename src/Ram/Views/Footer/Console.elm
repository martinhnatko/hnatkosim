module Ram.Views.Footer.Console exposing (..)

import Html exposing (Html)
import Html exposing (div, text)
import Html.Attributes exposing (id, class)
import Time

import Shared.Types.ConsoleMessage exposing (ConsoleMessage)
import Shared.Types.ConsoleMessageType exposing (ConsoleMessageType(..))

--Conole view
viewConsole : List ConsoleMessage -> Html msg
viewConsole consoleMessages =    
    div
        [ id "consoleContainer"
        , class "font-mono text-sm h-24 overflow-y-auto"
        ]
        (consoleMessages
            |> List.map (\msg ->

                case msg.messageType of
                    InfoMessage ->
                        div [ class "py-1" ]
                            [ text ("[" ++ formatTime msg.timestamp ++ "] " ++ msg.text) ]

                    ErrorMessage ->
                        div [ class "py-1 text-red-500" ]
                            [ text ("[" ++ formatTime msg.timestamp ++ "] " ++ msg.text) ]

                    SimStarted ->
                        div [ class "py-1 text-green-300" ]
                            [ text ("[" ++ formatTime msg.timestamp ++ "] " ++ msg.text) ]

                    SimStopped ->
                        div [ class "py-1 text-red-300" ]
                            [ text ("[" ++ formatTime msg.timestamp ++ "] " ++ msg.text) ]
            )
        )
        


formatTime : Time.Posix -> String
formatTime posix =
    let
        hh = Time.toHour Time.utc posix + 1
        mm = Time.toMinute Time.utc posix
        ss = Time.toSecond Time.utc posix
        twoDigits n = String.padLeft 2 '0' (String.fromInt n)
    in
    twoDigits hh ++ ":" ++ twoDigits mm ++ ":" ++ twoDigits ss