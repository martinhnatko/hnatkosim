module Am.Views.HeaderView exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (class)

import Am.Views.Header.ControlButtons exposing (controlButtons)
import Am.Views.Header.Menu exposing (menu)
import Am.Views.Header.SpeedSlider exposing (speedSlider)

import Am.Types.Messages exposing (Msg)
import Am.Types.Model exposing (Model)


headerView : Model -> Html Msg
headerView model =
    div [ class "flex gap-4" ]
        [ controlButtons model
        , speedSlider model.speedIdx model
        , menu
        ]
