module Shared.Components.SlotsModal exposing (..)

import Ram.Types.Messages exposing (Msg(..))

import Html exposing (Html, div, button, text, input, h2, p, span)
import Html.Attributes exposing (disabled, class, type_, value)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput, stopPropagationOn)

import Shared.Icons.X exposing (heroiconX)
import Shared.Icons.TrashSmall exposing (heroiconTrashSmall)
import Shared.Icons.Save exposing (heroiconSave)

import Array exposing (Array)
import Json.Decode as Decode

stopPropagationClick : msg -> Html.Attribute msg
stopPropagationClick noOpMsg =
    stopPropagationOn "click" <|
        Decode.succeed ( noOpMsg, True )


viewSlotsModal : Bool -> Array (String, Bool) -> msg -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> String -> msg ) -> msg -> Html msg
viewSlotsModal inputTextEmpty arrayOfSlots onToggleSlotsModal onSaveSlot onLoadSlot onDeleteSlot onUpdateSlotName onNoOp =
    div
        [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center"
        , onClick onToggleSlotsModal
        ]
        [ div
            [ class "bg-white p-4 rounded shadow-lg relative max-h-[80vh] max-w-[80vw] overflow-y-auto"
            , stopPropagationClick onNoOp
            ]
            [ -- “X” button in the top-right
              button
                [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700 focus:outline-none"
                , onClick onToggleSlotsModal
                ]
                [ heroiconX ]
            , h2 [ class "text-xl font-bold flex items-center gap-1" ] [ heroiconSave, text "Save/Load" ]
            , p [ class "text-sm mt-2" ]
                [ text "You can save and load code into separate slots. When you load a slot, its content is copied into your current workspace, so any edits only affect your workspace until you explicitly save them to a slot. In other words, your workspace is effectively its own slot as well. You can also rename your slots. After renaming, there’s no need to press save, as changes are saved automatically." ]
            , span [ class "text-red-500 text-sm" ] [text "Warning: Everything is stored in your browser’s local storage, so it’s unique to this browser and won’t carry over if you switch browsers or devices." ]
            , viewSlots inputTextEmpty arrayOfSlots onSaveSlot onLoadSlot onDeleteSlot onUpdateSlotName
            ]
        ]


viewSlots : Bool -> Array (String, Bool) -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> String -> msg ) -> Html msg
viewSlots inputTextEmpty arrayOfSlots onSaveSlot onLoadSlot onDeleteSlot onUpdateSlotName =
    div [ class "grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5 gap-3 mt-3" ]
        (List.map
            (\i ->
                let
                    (slotName, isEmpty) =
                        Array.get i arrayOfSlots |> Maybe.withDefault ("", True)
                in
                -- A small box for each slot
                div (if isEmpty then 
                        [ class "border p-3 rounded bg-gray-100 shadow-sm w-full" ]
                     else
                        [ class "border p-3 rounded bg-white shadow-sm w-full" ])
                    [ -- Editable slot name input:
                      input
                        [ type_ "text"
                        , class "font-bold text-gray-800 border border-gray-300 rounded p-1 w-full mb-1 focus:outline-none focus:ring focus:ring-blue-200"
                        , value slotName
                        , onInput (\newName -> onUpdateSlotName i newName)
                        , placeholder ("Slot " ++ String.fromInt i)
                        ]
                        []
                      -- Row of 3 buttons
                    , div [ class "flex gap-2 mt-1" ]
                        [ -- Save button
                          button
                            [ class 
                                ( if inputTextEmpty then
                                    "w-full bg-gray-500 text-white px-2 py-1 rounded opacity-50 cursor-not-allowed"
                                  else
                                    "w-full bg-blue-500 text-white px-2 py-1 rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                                )
                            , onClick (onSaveSlot i)
                            , disabled inputTextEmpty
                            ]
                            [ text "Save" ]
                          -- Load button
                        , button
                            [ class
                                ( if not isEmpty then
                                    "w-full bg-blue-500 text-white px-2 py-1 rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                                  else
                                    "w-full bg-gray-500 text-white px-2 py-1 rounded opacity-50 cursor-not-allowed"
                                )
                            , onClick (onLoadSlot i)
                            , disabled isEmpty
                            ]
                            [ text "Load" ]
                          -- Delete (trash icon) button
                        , button
                            [ class
                                ( if not isEmpty then
                                    "w-full bg-red-500 text-white px-2 py-1 rounded flex items-center justify-center hover:bg-red-600 transition-colors duration-200 focus:outline-none"
                                  else  
                                    "w-full bg-gray-500 text-white px-2 py-1 rounded flex items-center justify-center opacity-50 cursor-not-allowed"
                                )
                            , onClick (onDeleteSlot i)
                            , disabled isEmpty
                            ]
                            [ heroiconTrashSmall ]
                        ]
                    ]
            )
            (List.range 1 20)
        )
