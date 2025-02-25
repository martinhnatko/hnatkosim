module Am.Components.GuideModal exposing (..)

import Html exposing (Html, div, button, text, h2, p, ul, li, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Shared.Icons.X exposing (heroiconX)
import Html exposing (table)
import Html exposing (thead)
import Html exposing (tr)
import Html exposing (th)
import Html exposing (tbody)
import Html exposing (td)

viewGuideModal : msg -> (Int -> msg) -> Html msg
viewGuideModal onToggleGuideModal onLoadSlot =
    div
        [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center"
        ]
        [ div
            [ class "bg-white p-4 rounded shadow-lg relative max-h-[80vh] max-w-4xl overflow-y-auto"
            ]
            [ -- “X” button in the top-right
              button
                [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700"
                , onClick onToggleGuideModal
                ]
                [ heroiconX ]
            
            
                , h2 [ class "text-xl font-bold mb-4" ] [ text "Abacus Machine Guide" ]
                
                , p []
                    [ text "Abacus Machine is an abstract model of a simple computing device that operates with an unlimited number of registers, where each register can hold arbitrarily large natural numbers." ]
                
                , p []
                    [ span [ class "text-red-500 font-bold" ]
                        [ text "Errors " ]
                    , text " are printed to the console below. Comments in the code can be written using the '#' symbol." ]
                
                , p [ class "mt-4" ]
                    [ text "Operations:" ]
                
                , table [ class "min-w-full table-fixed mt-2" ]
                        [ thead []
                            [ tr []
                                [ th [ class "px-4 py-2 bg-gray-50 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ]
                                    [ text "Instruction" ]
                                , th [ class "px-4 py-2 bg-gray-50 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ]
                                    [ text "Example" ]
                                , th [ class "px-4 py-2 bg-gray-50 text-left text-xs font-medium text-gray-500 uppercase tracking-wider" ]
                                    [ text "Description" ]
                                ]
                            ]
                        , tbody [ class "bg-white divide-y divide-gray-200" ]
                            [ 
                            
                            -- Increment operation row
                            tr []
                                [ td [ class "px-4 py-2 whitespace-normal" ]
                                    [ span [ class "text-green-500 font-bold" ]
                                        [ text "Increment" ]
                                    ]
                                , td [ class "px-4 py-2 whitespace-normal text-center" ]
                                    [ text "a1" ]
                                , td [ class "px-4 py-2 whitespace-normal" ]
                                    [ text "The operation of adding one to register Ri is denoted by 'ai'. After executing this operation, the value in register Ri increases by 1 (i.e. Ri = Ri + 1)." ]
                                ]
                            
                            -- Decrement operation row
                            , tr []
                                [ td [ class "px-4 py-2 whitespace-normal" ]
                                    [ span [ class "text-yellow-500 font-bold" ]
                                        [ text "Decrement" ]
                                    ]
                                , td [ class "px-4 py-2 whitespace-normal text-center" ]
                                    [ text "s3" ]
                                , td [ class "px-4 py-2 whitespace-normal" ]
                                    [ text "The operation of subtracting one from register Ri is denoted by 'si'. It performs Ri = Ri - 1 only if the value in register Ri is non-zero; otherwise, the value remains unchanged." ]
                                ]
                            
                            -- Loop operation row
                            , tr []
                                [ td [ class "px-4 py-2 whitespace-normal" ]
                                    [ span [ class "text-gray-500 font-bold" ]
                                        [ text "Loop" ]
                                    ]
                                , td [ class "px-4 py-2 whitespace-normal text-center" ]
                                    [ text "(a7s8)8" ]
                                , td [ class "px-4 py-2 whitespace-normal" ]
                                    [ text "A loop is implemented with the notation '(<loop body>)<condition register>', where the value in the condition register is tested first. If non-zero, the loop body is executed repeatedly until the condition register becomes zero, at which point the loop terminates." ]
                                ]
                            ]
                        ]
                
                , p [ class "mt-4" ]
                    [ text "Example codes:" ]

                , div [ class "flex flex-row gap-4 mt-2" ] 
                    [
                    
                    
                    div [ class "border p-3 rounded bg-white shadow-sm w-full" ]
                        [ div
                            [ class "font-bold text-gray-700" ]
                            [ text "Example 1: a+b" ]

                        , div [ class "flex gap-2 mt-2" ]
                            [
                            -- Load button
                            button
                                [ class "bg-blue-500 text-white px-2 py-1 rounded"
                                , onClick (onLoadSlot 21)
                                ]
                                [ text "Load" ]
                            ]
                        ]

                    , div [ class "border p-3 rounded bg-white shadow-sm w-full" ]
                        [ div
                            [ class "font-bold text-gray-700" ]
                            [ text "Example 2: a*b" ]

                        , div [ class "flex gap-2 mt-2" ]
                            [
                            -- Load button
                            button
                                [ class "bg-blue-500 text-white px-2 py-1 rounded"
                                , onClick (onLoadSlot 22)
                                ]
                                [ text "Load" ]
                            ]
                        ]
                    
                    , div [ class "border p-3 rounded bg-white shadow-sm w-full" ]
                        [ div
                            [ class "font-bold text-gray-700" ]
                            [ text "Example 3: n!" ]

                        , div [ class "flex gap-2 mt-2" ]
                            [
                            -- Load button
                            button
                                [ class "bg-blue-500 text-white px-2 py-1 rounded"
                                , onClick (onLoadSlot 23)
                                ]
                                [ text "Load" ]
                            ]
                        ]

                    ]
            ]
        ]