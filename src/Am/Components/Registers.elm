module Am.Components.Registers exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Dict exposing (Dict)

viewRegisters : Dict Int (Int, Bool) -> Dict Int String -> Html msg
viewRegisters registers highlighted =
    div [ class "flex flex-col md:w-1/3 bg-white p-1 shadow-lg rounded md:overflow-auto" ]
        (
        registers
            |> Dict.toList
            |> List.map
                (\(regNum, (value, wasUsed)) ->
                    let
                        highlightClass =
                            Dict.get regNum highlighted
                                |> Maybe.withDefault ""
                    in
                    div
                        [ class
                            (
                                if wasUsed then
                                    "flex items-center gap-4 p-1 border-b last:border-none font-mono " 
                                    ++ highlightClass
                                else
                                    "flex items-center gap-4 p-1 border-b last:border-none font-mono bg-gray-100 rounded" 
                                    ++ highlightClass
                            )
                        ]
                        [ div [ class "text-gray-500 w-8 text-right" ]
                            [ text (String.fromInt regNum) ]
                        , div [ class "h-5 w-px bg-gray-300" ] []
                        , div [ class "flex-1 text-left font-medium text-gray-900" ]
                            [ text (String.fromInt value) ]
                        ]
                )
        )