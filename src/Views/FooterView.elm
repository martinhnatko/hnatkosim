module Views.FooterView exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Views.Footer.Console exposing (viewConsole)
import Types.Model exposing (Model)
import Types.Messages exposing (Msg)


footerView : Model -> Html Msg
footerView model =
    div [ class "bg-gray-800 text-white p-3 rounded shadow-lg" ]
        [ viewConsole model.consoleMessages ]
