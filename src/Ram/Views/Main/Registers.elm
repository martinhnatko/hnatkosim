module Ram.Views.Main.Registers exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Model exposing (Model)
import Array

viewRegisters : Model -> List (Html Msg)
viewRegisters model =
    model.registers
        |> Dict.toList
        |> List.map
            (\(regNum, value) ->
                let
                    highlightClass =
                        -- if model.speedIdx == (List.length (Array.toList model.speeds)) then
                        --     ""
                        -- else
                            Dict.get regNum model.highlighted_registers
                                |> Maybe.withDefault ""
                in
                div
                    [ class
                        ("flex items-center gap-3 p-2 border-b last:border-none font-mono " 
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