module Am.Components.SettingsModal exposing (viewSettingsModal)

import Shared.Icons.X exposing (heroiconX)
import Shared.Icons.Settings exposing (heroiconSettings)

import Html exposing (Html, div, button, text, input, h2, p, span, h3)
import Html.Attributes exposing (class, type_, value, disabled, placeholder)
import Html.Events exposing (onClick, onInput, stopPropagationOn)

import String
import Json.Decode as Decode

stopPropagationClick : msg -> Html.Attribute msg
stopPropagationClick noOpMsg =
    stopPropagationOn "click" <|
        Decode.succeed ( noOpMsg, True )

viewSettingsModal : Int -> Int -> msg -> (Int -> msg) -> (Int -> msg) -> (String -> msg) -> (String -> msg) -> String -> String -> msg -> Html msg
viewSettingsModal currentNumRegs currentMaxInstr onClose onChangeNumOfRegs onChangeExeInstructions onTypedNumRegs onTypedMaxInstr typedNumRegs typedMaxInstr onNoOp =
    div [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center"
        , onClick onClose
        ]
        [ div [ class "bg-white p-4 rounded shadow-lg relative max-w-xl w-full"
                , stopPropagationClick onNoOp
                ]
            [ -- Close Button (top-right)
              button
                [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700 focus:outline-none"
                , onClick onClose
                ]
                [ heroiconX ]

            , h2 [ class "text-xl font-bold mb-2 flex items-center gap-1" ] [ heroiconSettings ,text "Settings" ]

            -- Warning
            , p [ class "my-3 text-red-600 font-semibold" ]
                [ text "Warning: Changing these settings is not recommended, as they may degrade performance." ]

            -- Number of Registers
            , div [ class "mb-4" ] [ h3 [ class "text-md font-bold" ] [ text "Number of registers:" ]
                
                , div [ class "flex gap-2 items-center text-sm text-gray-600" ]
                    [ p [] [ text "min: 10" ]
                    , div [ class "h-4 w-px bg-gray-400" ] []
                    , p [] [ span [ class "text-green-600 font-semibold" ] [ text "default: 100" ] ]
                    , div [ class "h-4 w-px bg-gray-400" ] []
                    , p [] [ text "max: 10,000" ]
                    ]
                , div [ class "flex gap-2 items-center text-sm mb-2" ]
                    [ p [] [ span [ class "text-blue-600 font-semibold" ] [ text ("current: " ++ String.fromInt currentNumRegs) ] ] ]  

                , div [ class "flex gap-2 items-center" ]
                    [ input
                        [ type_ "number"
                        , Html.Attributes.min "10"
                        , Html.Attributes.max "10000"
                        , value typedNumRegs
                        , onInput onTypedNumRegs
                        , class "border rounded p-1 w-full"
                        , placeholder "Type here..."
                        ]
                        []
                    , let
                        parsed = Maybe.withDefault 0 (String.toInt typedNumRegs) 
                        isValid =
                            parsed >= 10 && parsed <= 10000
                      in
                      button
                        [ class
                            (if isValid then
                                "bg-blue-500 text-white px-3 py-1 rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                             else
                                "bg-gray-400 text-white px-3 py-1 rounded opacity-50 cursor-not-allowed"
                            )
                        , disabled (not isValid)
                        , onClick (onChangeNumOfRegs parsed)
                        ]
                        [ text "Set" ]
                    ]
                ]

            -- Max Executed Instructions
            , div [] [ h3 [ class "text-md font-bold" ] [ text "Maximum number of executed instructions with instant speed:" ]
                 , div [ class "flex gap-2 items-center text-sm text-gray-600" ]
                    [ p [] [ text "min: 500,000" ]
                    , div [ class "h-4 w-px bg-gray-400" ] []
                    , p [] [ span [ class "text-green-600 font-semibold" ] [ text "default: 1,000,000" ] ]
                    , div [ class "h-4 w-px bg-gray-400" ] []
                    , p [] [ text "max: 100,000,000" ]
                    ]
                , div [ class "flex gap-2 items-center text-sm mb-2" ]
                    [ p [] [ span [ class "text-blue-600 font-semibold" ] [ text ("current: " ++ String.fromInt currentMaxInstr) ] ] ]  
                , div [ class "flex gap-2 items-center" ]
                    [ input
                        [ type_ "number"
                        , Html.Attributes.min "500000"
                        , Html.Attributes.max "100000000"
                        , value typedMaxInstr
                        , onInput onTypedMaxInstr
                        , class "border rounded p-1 w-full"
                        , placeholder "Type here..."
                        ]
                        []
                    , let
                        parsed =
                            String.toInt typedMaxInstr
                                |> Maybe.withDefault 0

                        isValid =
                            parsed >= 500000 && parsed <= 100000000
                      in
                      button
                        [ class
                            (if isValid then
                                "bg-blue-500 text-white px-3 py-1 rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                             else
                                "bg-gray-400 text-white px-3 py-1 rounded opacity-50 cursor-not-allowed"
                            )
                        , disabled (not isValid)
                        , onClick (onChangeExeInstructions parsed)
                        ]
                        [ text "Set" ]
                    ]
                ]
            ]
        ]