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
    div [ class "flex bg-white rounded space-x-2 p-3 overflow-x-auto" ]
        ( (List.indexedMap
                (\index cell ->
                    renderInputCell index cell model
                )
                cellsList
          )
          ++ [ button
                [ class (if model.simStarted then
                            "w-20 h-20 text-transparent pointer-events-none flex items-center justify-center flex-shrink-0"
                        else
                            "w-20 h-20 border rounded bg-green-200 text-green-800 flex items-center justify-center flex-shrink-0"
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
                "bg-gray-100 cursor-not-allowed text-gray-700"
              else
                "bg-white"
            )
        
        commonAttrs =
            [ type_ "number"
            , class ("w-20 h-20 border rounded text-center appearance-none font-mono " ++ bgClass
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
                [ class "absolute top-0 right-0 w-5 h-5 bg-red-500 text-white rounded-full flex items-center justify-center text-xs cursor-pointer"
                , onClick RemoveLastCell
                ]
                [ heroiconX ]
            ]
    else
        inputElement