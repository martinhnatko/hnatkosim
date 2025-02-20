module Shared.Components.SlotsModal exposing (..)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)

import Array exposing (Array)

import Shared.Icons.X exposing (heroiconX)
import Shared.Icons.TrashSmall exposing (heroiconTrashSmall)

viewSlotsModal : String -> Array String -> msg -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> msg ) -> Html msg
viewSlotsModal inputText slots onToggleSlotsModal onSaveSlot onLoadSlot onDeleteSlot =
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

              , viewSlots inputText slots onSaveSlot onLoadSlot onDeleteSlot
            ]
        ]

viewSlots : String -> Array String -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> msg ) -> Html msg
viewSlots inputText slots onSaveSlot onLoadSlot onDeleteSlot =
    div [ Html.Attributes.class "grid grid-cols-5 gap-4 mt-8" ]
        (List.map
            (\i ->
                let
                    label =
                        "Slot " ++ String.fromInt i

                    maybeCode =
                        Array.get i slots

                    isEmpty =
                        case maybeCode of
                            Just code ->
                                code == ""

                            Nothing ->
                                True
                in
                -- A small box for each slot
                div [ Html.Attributes.class "border p-3 rounded bg-white shadow-sm w-full" ]
                    [ -- The slot label
                      div [ Html.Attributes.class "font-bold text-gray-700" ]
                        [ text label ]

                      -- The row of 3 buttons
                    , div [ Html.Attributes.class "flex gap-2 mt-2" ]
                        [ -- Save button
                          button
                            [ Html.Attributes.class 
                                ( "bg-blue-500 text-white px-2 py-1 rounded"
                                    ++ if inputText == "" then
                                        " opacity-50 cursor-not-allowed"
                                    else
                                        ""
                                )
                            , onClick (onSaveSlot i)
                            ]
                            [ text "Save" ]

                          -- Load button
                        , button
                            [ Html.Attributes.class
                                ( "bg-blue-500 text-white px-2 py-1 rounded"
                                    ++ if isEmpty then
                                        " opacity-50 cursor-not-allowed"
                                       else
                                        ""
                                )
                            , onClick (onLoadSlot i)
                            , disabled isEmpty
                            ]
                            [ text "Load" ]

                          -- Delete (trash icon)
                        , button
                            [ Html.Attributes.class
                                ( "bg-red-500 text-white px-2 py-1 rounded flex items-center justify-center"
                                    ++ if isEmpty then
                                        " opacity-50 cursor-not-allowed"
                                       else
                                        ""
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

