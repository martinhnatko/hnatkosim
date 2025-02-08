module Ram.Views.Main.InputTape exposing (viewInputTape)

import Array
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, disabled, id, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, onMouseDown)
import Ram.Types.Messages exposing (Msg(..))
import Ram.Types.Model exposing (Model)
import String
import Shared.Icons.XSmall exposing (heroiconX)


{-| Renders an individual tape cell.
    For a cell that is Nothing, if it is the first empty cell then:
      - when simulation is not started, it shows a green cell with a plus overlay,
      - when simulation is started, it shows a default placeholder ("0") so that a cell is visible.
-}
renderInputCell : Int -> Maybe Int -> Model -> Int -> Int -> Html Msg
renderInputCell index cell model firstEmptyIndex lastFilledIndex =
    let
        ( displayValue, bgClass, isEditable ) =
            case cell of
                Nothing ->
                    if index == firstEmptyIndex then
                        if model.simStarted then
                            -- Show a default placeholder so that the tape isn’t empty
                            ( "", "invisible", False )
                        else
                            ( "", "bg-green-200", True )
                    else
                        ( "", "", False )

                Just n ->
                    ( String.fromInt n
                    , if model.simStarted then
                        "bg-gray-200 cursor-not-allowed text-gray-500"
                      else
                        "bg-white"
                    , True
                    )

        isActive =
            model.inputTapePointer == index && index /= firstEmptyIndex

        commonAttrs =
            [ id ("input-" ++ String.fromInt index)
            , type_ "number"
            , class ("w-20 h-20 border rounded text-center appearance-none font-mono " ++ bgClass
                      ++ (if isActive then " border-blue-500 border-4 font-bold" else "")
              )
            , value displayValue
            , onBlur CellBlurred
            , onMouseDown (CellFocused index)
            ]

        editableAttrs =
            if isEditable then
                [ onInput (\s ->
                      case String.toInt s of
                          Just n ->
                              UpdateInputTape index n

                          Nothing ->
                              UpdateInputTape index 0
                  )
                ]
            else
                [ disabled True ]

        simDisabled =
            if model.simStarted then
                [ disabled True ]
            else
                []

        inputElement =
            input (commonAttrs ++ editableAttrs ++ simDisabled) []
    in
    if index == lastFilledIndex && model.maybeFocusedCell /= Just index && not model.simStarted then
        -- Render a removal (×) overlay on the last filled cell
        div [ class "relative" ]
            [ inputElement
            , button
                [ class "absolute top-0 right-0 w-5 h-5 bg-red-500 text-white rounded-full flex items-center justify-center text-xs cursor-pointer"
                , onClick (RemoveCell index)
                ]
                [ heroiconX ]
            ]
    else if index == firstEmptyIndex && model.maybeFocusedCell /= Just index then
        if not model.simStarted then
            -- Render an add (+) overlay when not simulating
            div [ class "relative" ]
                [ inputElement
                , button
                    [ class "absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-[59%] text-green-500 text-4xl font-bold pointer-events-none" ]
                    [ text "+" ]
                ]
        else
            -- When simulating, just show the input so the tape isn’t completely empty.
            inputElement
    else if isEditable then
        inputElement
    else
        text ""


{-| Renders the full input tape.
    It converts the input tape (an Array of Maybe Int) to a list,
    determines the first empty cell and the last filled cell,
    and then renders each cell accordingly.
-}
viewInputTape : Model -> Html Msg
viewInputTape model =
    let
        cellsList : List (Maybe Int)
        cellsList =
            Array.toList model.inputTape

        firstEmptyIndex : Int
        firstEmptyIndex =
            cellsList
                |> List.indexedMap Tuple.pair
                |> List.filter (\(_, cell) -> cell == Nothing)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0

        lastFilledIndex : Int
        lastFilledIndex =
            if firstEmptyIndex > 0 then
                firstEmptyIndex - 1
            else
                -1
    in
    div [ class "flex bg-white rounded space-x-2 p-3 overflow-x-auto" ]
        (List.indexedMap
            (\index cell ->
                renderInputCell index cell model firstEmptyIndex lastFilledIndex
            )
            cellsList
        )
