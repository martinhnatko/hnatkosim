module Views.SaveSlots exposing (..)
import Html exposing (Html)
import Html exposing (div)
import Html.Attributes
import Html exposing (button)
import Html.Events exposing (onClick)
import Icons.X exposing (heroiconX)
import Icons.TrashSmall exposing (heroiconTrashSmall)
import Array
import Html exposing (text)
import Html.Attributes exposing (disabled)
import Types.Messages exposing (Msg(..))
import Types.Model exposing (Model)

viewSlotsModal : Model -> Html Msg
viewSlotsModal model =
    div
        [ Html.Attributes.class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center"
        ]
        [ div
            [ Html.Attributes.class "bg-white p-4 rounded shadow-lg relative"
            ]
            [ -- “X” button in the top-right
              button
                [ Html.Attributes.class "absolute top-2 right-2 text-gray-500 hover:text-gray-700"
                , onClick ToggleSlotsModal
                ]
                [ heroiconX ]

              , viewSlots model
            ]
        ]

viewSlots : Model -> Html Msg
viewSlots model =
    div [ Html.Attributes.class "grid grid-cols-5 gap-4 mt-8" ]
        (List.map
            (\i ->
                let
                    label =
                        "Slot " ++ String.fromInt i

                    maybeCode =
                        Array.get i model.slots

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
                                    ++ if model.inputText == "" then
                                        " opacity-50 cursor-not-allowed"
                                    else
                                        ""
                                )
                            , onClick (SaveSlot i)
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
                            , onClick (LoadSlot i)
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
                            , onClick (DeleteSlot i)
                            , disabled isEmpty
                            ]
                            [ heroiconTrashSmall ]
                        ]
                    ]
            )
            (List.range 1 20)
        )

