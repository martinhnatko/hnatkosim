module Shared.Components.SlotsModal exposing (..)

import Ram.Types.Messages exposing (Msg(..))

import Html exposing (Html, div, button, text, input, h2, p, span, strong)
import Html.Attributes exposing (disabled, class, type_, value)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput, stopPropagationOn)

import Shared.Icons.X exposing (heroiconX)
import Shared.Icons.TrashSmall exposing (heroiconTrashSmall)
import Shared.Icons.Save exposing (heroiconSave)
import Shared.Icons.Download exposing (heroiconDownload)
import Shared.Icons.Upload exposing (heroiconUpload)

import Array exposing (Array)
import Json.Decode as Decode

stopPropagationClick : msg -> Html.Attribute msg
stopPropagationClick noOpMsg =
    stopPropagationOn "click" <|
        Decode.succeed ( noOpMsg, True )


viewSlotsModal : Bool -> Array (String, Bool) -> msg -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> String -> msg ) -> msg -> ( Int -> msg ) -> ( Int -> msg ) -> Html msg
viewSlotsModal inputTextEmpty arrayOfSlots onToggleSlotsModal onSaveSlot onLoadSlot onDeleteSlot onUpdateSlotName onNoOp onTriggerUpload onTriggerDownload =
    div
        [ class "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center"
        , onClick onToggleSlotsModal
        ]
        [ div
            [ class "bg-white p-3 rounded shadow-lg relative max-h-[80vh] max-w-[80vw] overflow-y-auto"
            , stopPropagationClick onNoOp
            ]
            [ -- \"X\" button in the top-right
              button
                [ class "absolute top-2 right-2 text-gray-500 hover:text-gray-700 focus:outline-none"
                , onClick onToggleSlotsModal
                ]
                [ heroiconX ]
            , h2 [ class "text-xl font-bold flex items-center gap-1" ] [ heroiconSave, text "Save/Load" ]
            , p [ class "text-sm mt-2" ]
                [ text "You can "
                , span [class "font-semibold"] [ text "save" ]
                , text " and "
                , span [class "font-semibold"] [ text "load" ]
                , text " code into separate slots. When you load a slot, its content is copied into your current workspace, so any edits affect only your workspace until you explicitly save them to a slot. In other words, your workspace is effectively its own slot as well. You can also rename your slots. After renaming, there's no need to press save, as changes are saved automatically." ]
            , p [ class "text-red-500 text-sm my-0.5" ] 
                [text "Warning: Everything is stored in your browser's local storage, so it's unique to this browser and won't carry over if you switch browsers or devices." ]
            , p [ class "text-sm" ]
                [ text "You also have the ability to "
                , span [class "font-semibold"] [ text "download" ]
                , text " or "
                , span [class "font-semibold"] [ text "upload" ]
                , text " code to specific slots. When downloading, the code from a slot will be downloaded as the content of a file named "
                , span [class "font-mono"] [text "<slot-name>.txt"]
                , text ". When uploading, you need to select a " 
                , span [class "font-mono"] [text ".txt"]
                , text " file from your device, which will then be saved to the corresponding slot." ]
            , viewSlots inputTextEmpty arrayOfSlots onSaveSlot onLoadSlot onDeleteSlot onUpdateSlotName onTriggerUpload onTriggerDownload
            ]
        ]


viewSlots : Bool -> Array (String, Bool) -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> msg ) -> ( Int -> String -> msg ) -> ( Int -> msg ) -> ( Int -> msg ) -> Html msg
viewSlots inputTextEmpty arrayOfSlots onSaveSlot onLoadSlot onDeleteSlot onUpdateSlotName onTriggerUpload onTriggerDownload =
    div [ class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 2xl:grid-cols-5 gap-3.5 mt-3" ]
        (List.map
            (\i ->
                let
                    (slotName, isEmpty) =
                        Array.get i arrayOfSlots |> Maybe.withDefault ("", True)
                in
                -- A small box for each slot
                div (if isEmpty then 
                        [ class "flex flex-col gap-2 border p-2.5 rounded bg-gray-200 shadow-sm w-full" ]
                     else
                        [ class "flex flex-col gap-2 border p-2.5 rounded bg-white shadow-sm w-full" ])
                    [ -- Editable slot name input:
                    input
                      [ type_ "text"
                      , class "font-bold text-gray-800 border border-gray-300 rounded p-1 w-full focus:outline-none focus:ring focus:ring-blue-200"
                      , value slotName
                      , onInput (\newName -> onUpdateSlotName i newName)
                      , placeholder ("Slot " ++ String.fromInt i)
                      ]
                      []
                      -- Row of 3 buttons
                    , div [ class "flex gap-2 h-8" ]
                        [ -- Save button
                          button
                            [ class 
                                ( if inputTextEmpty then
                                    "bg-gray-500 text-white w-1/3 rounded opacity-50 cursor-not-allowed"
                                  else
                                    "bg-blue-500 text-white w-1/3 rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                                )
                            , onClick (onSaveSlot i)
                            , disabled inputTextEmpty
                            ]
                            [ text "Save" ]
                          -- Load button
                        , button
                            [ class
                                ( if not isEmpty then
                                    "bg-blue-500 text-white w-1/3 rounded hover:bg-blue-600 transition-colors duration-200 focus:outline-none"
                                  else
                                    "bg-gray-500 text-white w-1/3 rounded opacity-50 cursor-not-allowed"
                                )
                            , onClick (onLoadSlot i)
                            , disabled isEmpty
                            ]
                            [ text "Load" ]
                        , button
                            [ class
                                ( if not isEmpty then
                                    "bg-red-500 text-white w-1/3 rounded flex items-center justify-center hover:bg-red-600 transition-colors duration-200 focus:outline-none"
                                  else  
                                    "bg-gray-500 text-white w-1/3 rounded flex items-center justify-center opacity-50 cursor-not-allowed"
                                )
                            , onClick (onDeleteSlot i)
                            , disabled isEmpty
                            ]
                            [ heroiconTrashSmall ]
                          
                        
                        ]
                      , div [class "flex flex-row gap-2 h-6 text-sm"][
                        button
                              [ class
                                  ( if not isEmpty then
                                      "border gap-1 px-1 py-1 border-blue-500 text-blue-500 bg-white hover:bg-blue-50 w-1/2 rounded flex items-center justify-center transition-colors duration-200 focus:outline-none"
                                    else  
                                      "border gap-1 px-1 py-1 border-gray-400 text-gray-400 bg-gray-100 w-1/2 rounded flex items-center justify-center cursor-not-allowed"
                                  )
                              , onClick (onTriggerDownload i)
                              , disabled isEmpty
                              ]
                              [ heroiconDownload, text "Download" ]
                        , button
                              [ class "border gap-1 px-1 py-1 border-blue-500 text-blue-500 bg-white hover:bg-blue-50 w-1/2 rounded flex items-center justify-center transition-colors duration-200 focus:outline-none"
                                    
                              , onClick (onTriggerUpload i)
                              ]
                              [ heroiconUpload, text "Upload" ]
                      ]                          
                          
                    ]
            )
            (List.range 1 20)
        )
