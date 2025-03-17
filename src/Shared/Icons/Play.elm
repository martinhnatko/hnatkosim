module Shared.Icons.Play exposing (..)
import Html exposing (Html)
import Svg exposing (svg, path)
import Svg.Attributes exposing (fill, stroke, strokeWidth, viewBox, strokeLinecap, strokeLinejoin, d, class)

heroiconPlay : Html msg
heroiconPlay =
    svg
        [ class "h-5 w-5"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.5"
        , viewBox "0 0 24 24"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path [ d "M5.25 5.25v13.5l10.5-6.75-10.5-6.75z" ] [] ]