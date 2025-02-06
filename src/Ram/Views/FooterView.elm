module Ram.Views.FooterView exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Ram.Views.Footer.Console exposing (viewConsole)
import Ram.Types.Model exposing (Model)
import Ram.Types.Messages exposing (Msg)


footerView : Model -> Html Msg
footerView model =
    div [ class "bg-gray-800 text-white p-3 rounded shadow-lg" ]
        [ viewConsole model.consoleMessages ]
