module Ram.Components.Registers exposing (..)

import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Model exposing (Model)

import Html exposing (Html, div, text, h3, p, span)
import Html.Attributes exposing (class)

import Dict

viewRegisters : Model -> Html Msg
viewRegisters model =
    div [class "flex flex-col md:w-1/3 gap-2"] [
        
        div [class "flex flex-col w-full h-full overflow-y-auto bg-white rounded p-1.5 order-2 md:order-1"] 
            
                (
                model.registers
                    |> Dict.toList
                    |> List.map
                        (\(regNum, (value, wasUsed)) ->
                            let
                                highlightClass =
                                        Dict.get regNum model.highlighted_registers
                                            |> Maybe.withDefault ""
                            in
                            div
                                [ 
                                class
                                    (
                                    case wasUsed of
                                        Just _ ->
                                            "flex items-center gap-4 p-0.5 border-b last:border-none font-mono "
                                                ++ highlightClass

                                        Nothing ->
                                            "flex items-center gap-4 p-0.5 border-b last:border-none font-mono bg-gray-100 rounded " 
                                                ++ highlightClass
                                    )
                                ]
                                [ div 
                                    (
                                    if regNum == 0 then
                                        [ class "text-blue-500 w-8 text-right" ]
                                    else 
                                        [ class "text-gray-500 w-8 text-right" ]
                                    )
                                    [ text (String.fromInt regNum) ]
                                , div [ class "h-5 w-px bg-gray-300" ] []
                                , div [ class "flex-1 text-left font-medium text-gray-900" ]
                                    [ text (String.fromInt value) ]
                                ]
                        )
                    )   
 
            -- ] 
        
        , div [ class "flex w-full max-h-[30%] bg-white rounded p-1.5 rounded gap-1.5 order-1 md:order-2" ]
            [ 
            -- Card #1: Time Complexity
            div [ class "bg-white w-1/2 p-1 rounded border overflow-x-auto" ]
                [ h3 [ class "text-sm font-semibold" ] [ text "Time Complexity" ]
                , p [ class "text-xs" ] 
                    [ text "Unit cost: "
                    , span [ class "text-green-600 font-semibold" ] [ text (model.executedInstructions |> String.fromInt) ]   
                    ]
                , p [ class "text-xs" ] 
                    [ text "Logarithmic cost: " 
                    , span [ class "text-green-600 font-semibold" ] [ text (model.logTime |> String.fromInt) ]
                    ]
                ]
            
            -- Card #2: Space Complexity
            , div [ class "bg-white w-1/2 p-1 rounded border overflow-x-auto" ]
                [ h3 [ class "text-sm font-semibold" ] [ text "Space Complexity" ]
                , p [ class "text-xs" ] 
                    [ text "Unit cost: " 
                    , span [ class "text-green-600 font-semibold" ] [ text ( 
                                                                            model.registers
                                                                                |> Dict.values
                                                                                |> List.filter (\(_, wasUsed) -> wasUsed /= Nothing)
                                                                                |> List.map Tuple.second
                                                                                |> List.length
                                                                                |> String.fromInt
                                                                            ) 
                                                                    ]
                    ]
                , p [ class "text-xs" ] 
                    [ text "Logarithmic cost: " 
                    , span [ class "text-green-600 font-semibold" ] [ text (model.logSpace |> String.fromInt) ]
                    ]
                ]
            ]
        
    ]