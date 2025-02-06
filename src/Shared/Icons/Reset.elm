module Shared.Icons.Reset exposing (..)
import Html exposing (Html)
import Svg exposing (svg, path)
import Svg.Attributes exposing (fill, stroke, strokeWidth, viewBox, strokeLinecap, strokeLinejoin, d, class)

heroiconReset : Html msg
heroiconReset =
    svg
        [ class "h-6 w-6"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.5"
        , viewBox "0 0 24 24"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path [ d "M5.25 7.5A2.25 2.25 0 0 1 7.5 5.25h9a2.25 2.25 0 0 1 2.25 2.25v9a2.25 2.25 0 0 1-2.25 2.25h-9a2.25 2.25 0 0 1-2.25-2.25v-9Z" ] [] ]