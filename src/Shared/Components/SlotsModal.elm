module Shared.Components.SlotsModal exposing (..)

import Html exposing (Html, div, button, text, input, h2)
import Html.Attributes exposing (disabled, class, type_, value)
import Html.Events exposing (onClick, onInput)

import Array exposing (Array)

import Shared.Icons.X exposing (heroiconX)
import Shared.Icons.TrashSmall exposing (heroiconTrashSmall)

viewSlotsModal : Bool -> Array (String, Bool) -> msg -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> String -> msg ) -> Html msg
viewSlotsModal inputTextEmpty arrayOfSlots onToggleSlotsModal onSaveSlot onLoadSlot onDeleteSlot onUpdateSlotName =
    div
        [ Html.Attributes.class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center"
        ]
        [ div
            [ Html.Attributes.class "bg-white p-4 rounded shadow-lg relative"
            ]
            [ -- “X” button in the top-right
              button
                [ Html.Attributes.class "absolute top-2 right-2 text-gray-500 hover:text-gray-700"
                , onClick onToggleSlotsModal
                ]
                [ heroiconX ]

              , h2 [ class "text-xl font-bold" ] [ text "Save/Load" ]

              , viewSlots inputTextEmpty arrayOfSlots onSaveSlot onLoadSlot onDeleteSlot onUpdateSlotName
            ]
        ]

viewSlots : Bool -> Array (String, Bool) -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> String -> msg ) -> Html msg
viewSlots inputTextEmpty arrayOfSlots onSaveSlot onLoadSlot onDeleteSlot onUpdateSlotName =
    div [ Html.Attributes.class "grid grid-cols-5 gap-4 mt-3" ]
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
                        , class "font-bold text-gray-700 border border-gray-300 rounded p-1 w-full mb-2"
                        , value slotName
                        , onInput (\newName -> onUpdateSlotName i newName)
                        ]
                        []

                      -- The row of 3 buttons
                    , div [ class "flex gap-2 mt-2" ]
                        [ -- Save button
                          button
                            [ class 
                                ( if inputTextEmpty then
                                    "bg-gray-500 text-white px-2 py-1 rounded opacity-50 cursor-not-allowed"
                                  else
                                    "bg-blue-500 text-white px-2 py-1 rounded"
                                ) 
    
                            , onClick (onSaveSlot i)
                            , disabled inputTextEmpty
                            ]
                            [ text "Save" ]

                          -- Load button
                        , button
                            [ class
                                ( if not isEmpty then
                                    "bg-blue-500 text-white px-2 py-1 rounded"
                                  else
                                    "bg-gray-500 text-white px-2 py-1 rounded opacity-50 cursor-not-allowed"
                                )
                            , onClick (onLoadSlot i)
                            , disabled isEmpty
                            ]
                            [ text "Load" ]

                          -- Delete (trash icon)
                        , button
                            [ class
                                ( if not isEmpty then
                                    "bg-red-500 text-white px-2 py-1 rounded flex items-center justify-center"
                                  else  
                                    "bg-gray-500 text-white px-2 py-1 rounded flex items-center justify-center opacity-50 cursor-not-allowed"
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

