module Views.Main.InputText exposing (inputTextArea)

import Html exposing (Html, div, textarea, button, text)
import Html.Attributes exposing (class, placeholder, value, disabled, rows)
import Html.Events exposing (onInput, onClick)
import Icons.Trash exposing (heroiconTrash)
import Types.Messages exposing (Msg(..))
import Types.Model exposing (Model)


inputTextArea : Model -> Html Msg
inputTextArea model =
    div [ class "flex flex-col w-1/3 bg-white p-3 shadow-lg rounded relative" ]
        [ textarea
            ( [ class 
                    ( "flex-grow w-full h-full p-2 border rounded resize-none overflow-auto text-lg font-mono "
                        ++ if model.simStarted then
                            "bg-gray-200 text-gray-500 cursor-not-allowed"
                        else
                            "bg-white text-black"
                    )
            , placeholder "Enter your code here..."
            , onInput UpdateCode
            , value model.inputText
            , rows 10
            ]
            ++ (if model.simStarted then [ disabled True ] else [])
            )
            []
        , if not model.simStarted then
              button 
                  [ class "absolute bottom-9 right-10 text-gray-500 hover:text-red-500"
                  , onClick DeleteInput
                  ]
                  [ heroiconTrash ]
          else
              text ""
        ]
