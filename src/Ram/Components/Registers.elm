module Ram.Components.Registers exposing (..)

import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Model exposing (Model)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Dict

viewRegisters : Model -> Html msg
viewRegisters model =
    div [ class "flex flex-col w-1/3 bg-white p-1 shadow-lg rounded overflow-auto" ]
        (
        model.registers
            |> Dict.toList
            |> List.map
                (\(regNum, value) ->
                    let
                        highlightClass =
                                Dict.get regNum model.highlighted_registers
                                    |> Maybe.withDefault ""
                    in
                    div
                        [ 
                        class
                            ("flex items-center gap-4 p-1 border-b last:border-none font-mono " 
                                ++ highlightClass
                            )
                        ]
                        [ div 
                            -- (
                            -- if regNum == 0 then
                            --     [ class "text-blue-500 w-8 text-right" ]
                            -- else 
                                [ class "text-gray-500 w-8 text-right" ]
                            -- )
                            [ text (String.fromInt regNum) ]
                        , div [ class "h-5 w-px bg-gray-300" ] []
                        , div [ class "flex-1 text-left font-medium text-gray-900" ]
                            [ text (String.fromInt value) ]
                        ]
                )
        )