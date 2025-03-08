module Shared.Components.SlotsModal exposing (..)

import Ram.Types.Messages exposing (Msg(..))

import Html exposing (Html, div, button, text, input, h2)
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
        [ Html.Attributes.class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center"
        , onClick onToggleSlotsModal
        ]
        [ div
            [ Html.Attributes.class "bg-white p-4 rounded shadow-lg relative"
            , stopPropagationClick onNoOp
            ]
            [ -- “X” button in the top-right
              button
                [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700 focus:outline-none"
                , onClick onToggleSlotsModal
                ]
                [ heroiconX ]
            , h2 [ class "text-xl font-bold mb-4 flex items-center gap-1" ] [ heroiconSave, text "Save/Load" ]
            , viewSlots inputTextEmpty arrayOfSlots onSaveSlot onLoadSlot onDeleteSlot onUpdateSlotName
            ]
        ]


viewSlots : Bool -> Array (String, Bool) -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> String -> msg ) -> Html msg
viewSlots inputTextEmpty arrayOfSlots onSaveSlot onLoadSlot onDeleteSlot onUpdateSlotName =
    div [ class "grid grid-cols-5 gap-4 mt-3" ]
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
                        , class "font-bold text-gray-700 border border-gray-300 rounded p-1 w-full mb-2 focus:outline-none focus:ring focus:ring-blue-200"
                        , value slotName
                        , onInput (\newName -> onUpdateSlotName i newName)
                        , placeholder ("Slot " ++ String.fromInt i)
                        ]
                        []
                      -- Row of 3 buttons
                    , div [ class "flex gap-2 mt-2" ]
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
