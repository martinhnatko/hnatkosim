module Shared.Components.Console exposing (viewConsole, ConsoleMessage, ConsoleMessageType(..))

import Html exposing (Html, div, text)
import Html.Attributes exposing (id, class)

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
    | Warning

--Conole view
viewConsole : List ConsoleMessage -> Html msg
viewConsole consoleMessages =    
    div
        [ id "consoleContainer"
        , class "flex-none bg-gray-800 text-white p-1.5 rounded shadow-lg font-mono text-xs h-16 lg:h-20 overflow-y-auto"
        ]
        (consoleMessages
            |> List.map (\msg ->

                case msg.messageType of
                    InfoMessage ->
                        div [ ]
                            [ text ("[" ++ formatTime msg.timestamp ++ "] " ++ msg.text) ]

                    ErrorMessage ->
                        div [ class "text-red-500" ]
                            [ text ("[" ++ formatTime msg.timestamp ++ "] " ++ msg.text) ]

                    SimStarted ->
                        div [ class "text-green-300" ]
                            [ text ("[" ++ formatTime msg.timestamp ++ "] " ++ msg.text) ]

                    SimStopped ->
                        div [ class "text-red-300" ]
                            [ text ("[" ++ formatTime msg.timestamp ++ "] " ++ msg.text) ]
                    
                    Warning ->
                        div [ class "text-yellow-300" ]
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