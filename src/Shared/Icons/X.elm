module Shared.Icons.X exposing (..)
import Html exposing (Html)
import Svg exposing (svg, path)
import Svg.Attributes exposing (fill, stroke, strokeWidth, viewBox, strokeLinecap, strokeLinejoin, d, class)

-- The x icon
heroiconX : Html msg
heroiconX =
    svg
        [ class "h-10 w-10"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.5"
        , viewBox "0 0 24 24"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path 
            [ d "M6 18 18 6M6 6l12 12" ]
            []
        ]