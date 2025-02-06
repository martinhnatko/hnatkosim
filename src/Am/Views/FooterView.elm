module Am.Views.FooterView exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Am.Views.Footer.Console exposing (viewConsole)
import Am.Types.Model exposing (Model)
import Am.Types.Messages exposing (Msg)


footerView : Model -> Html Msg
footerView model =
    div [ class "bg-gray-800 text-white p-3 rounded shadow-lg" ]
        [ viewConsole model.consoleMessages ]
