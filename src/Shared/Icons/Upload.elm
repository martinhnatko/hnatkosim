module Shared.Icons.Upload exposing (..)

import Html exposing (Html)
import Svg exposing (svg, path)
import Svg.Attributes exposing (fill, stroke, strokeWidth, viewBox, strokeLinecap, strokeLinejoin, d, class)

heroiconUpload : Html msg
heroiconUpload =
    svg
        [ class "h-3 w-3"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.5"
        , viewBox "0 0 24 24"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path 
            [ d "M3 16.5v2.25A2.25 2.25 0 0 0 5.25 21h13.5A2.25 2.25 0 0 0 21 18.75V16.5m-13.5-9L12 3m0 0 4.5 4.5M12 3v13.5" ]
            []
        ]