module Ram.Views.HeaderView exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (class)

import Ram.Views.Header.ControlButtons exposing (controlButtons)
import Ram.Views.Header.Menu exposing (menu)
import Ram.Views.Header.SpeedSlider exposing (speedSlider)

import Ram.Types.Messages exposing (Msg)
import Ram.Types.Model exposing (Model)


headerView : Model -> Html Msg
headerView model =
    div [ class "flex gap-4" ]
        [ controlButtons model
        , speedSlider model.speedIdx model
        , menu
        ]
