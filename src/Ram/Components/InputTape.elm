module Ram.Components.InputTape exposing (viewInputTape)

import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Model exposing (Model)

import Shared.Icons.XSmall exposing (heroiconX)
import Shared.Icons.Plus exposing (heroiconPlus)

import Html exposing (Html, button, div, input)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput)

import Array
import String
import Dict

viewInputTape : Model -> Html Msg
viewInputTape model =
    let
        cellsList : List Int
        cellsList =
            Array.toList model.inputTape
    in
    div [ class "flex bg-white rounded space-x-1.5 p-1 lg:space-x-2 lg:p-2 overflow-x-auto" ]
        ( (List.indexedMap
                (\index cell ->
                    renderInputCell index cell model
                )
                cellsList
          )
          ++ [ button
                [ class (if model.simStarted then
                            "w-16 h-16 lg:w-20 lg:h-20 text-transparent pointer-events-none flex items-center justify-center flex-shrink-0"
                        else
                            "w-16 h-16 lg:w-20 lg:h-20 border rounded bg-green-200 text-green-800 flex items-center justify-center flex-shrink-0 hover:bg-green-300 hover:text-green-900 transition-colors duration-200"
                        )
                , onClick AddCellToInputTape
                ]
                [ heroiconPlus ]
             ]

        )

renderInputCell : Int -> Int -> Model -> Html Msg
renderInputCell index cell model =
    let
        highlightClass =
            Dict.get index model.highlighted_input_tape
                |> Maybe.withDefault ""

        ( displayValue, bgClass ) =
            ( String.fromInt cell
            , if highlightClass /= "" then
                highlightClass ++ " cursor-not-allowed text-gray-700"
              else if model.simStarted then
                "cursor-not-allowed text-gray-700"
              else
                "bg-white"
            )
        
        commonAttrs =
            [ type_ "number"
            , class ("w-16 h-16 lg:w-20 lg:h-20 border rounded text-center appearance-none font-mono " ++ bgClass
                      ++ (if model.inputTapePointer == index then " border-blue-500 border-4 font-bold" else "")
              )
            , value displayValue
            ]

        editableAttrs =
            [ onInput (\s ->
                    case String.toInt s of
                        Just n ->
                            UpdateInputTape index n

                        Nothing ->
                            UpdateInputTape index 0
                )
            ]

        isDisabled =
            if model.simStarted then
                [ disabled True ]
            else
                []

        inputElement =
            input (commonAttrs ++ editableAttrs ++ isDisabled) []
    in
    if index == Array.length model.inputTape - 1 && not model.simStarted then
        div [ class "relative" ]
            [ inputElement
            , button
                [ class "absolute top-0 right-0 w-5 h-5 bg-gray-300 text-white rounded-full flex items-center justify-center text-xs cursor-pointer hover:bg-red-500 hover:text-white transition-colors duration-200"
                , onClick RemoveLastCell
                ]
                [ heroiconX ]
            ]
    else
        inputElement