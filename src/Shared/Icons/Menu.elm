module Shared.Icons.Menu exposing (..)

import Html exposing (Html)
import Svg exposing (svg, path)
import Svg.Attributes exposing (fill, stroke, strokeWidth, viewBox, strokeLinecap, strokeLinejoin, d, class)


-- The save
heroiconMenu : Html msg
heroiconMenu =
    svg
        [ class "h-6 w-6"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.5"
        , viewBox "0 0 24 24"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path 
            [ d "M3.75 6.75h16.5M3.75 12h16.5m-16.5 5.25h16.5" ]
            []
        ]
