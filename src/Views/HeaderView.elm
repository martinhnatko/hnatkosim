module Views.HeaderView exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (class)

import Views.Header.ControlButtons exposing (controlButtons)
import Views.Header.Menu exposing (menu)
import Views.Header.SpeedSlider exposing (speedSlider)

import Types.Messages exposing (Msg)
import Types.Model exposing (Model)


headerView : Model -> Html Msg
headerView model =
    div [ class "flex gap-4" ]
        [ controlButtons model
        , speedSlider model.speedIdx model
        , menu
        ]
