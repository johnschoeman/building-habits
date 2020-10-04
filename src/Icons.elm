module Icons exposing (..)

import Html exposing (Html)
import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (..)


check : String -> Int -> Html msg
check color size =
    svgFeatherIcon color
        size
        [ Svg.polyline [ points "20 6 9 17 4 12" ] []
        ]


undo : String -> Int -> Html msg
undo color size =
    svgFeatherIcon color
        size
        [ Svg.polyline [ points "1 4 1 10 7 10" ] []
        , Svg.path [ d "M3.51 15a9 9 0 1 0 2.13-9.36L1 10" ] []
        ]


save : String -> Int -> Html msg
save color size =
    svgFeatherIcon color
        size
        [ Svg.path [ d "M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z" ] []
        , Svg.polyline [ points "17 21 17 13 7 13 7 21" ] []
        , Svg.polyline [ points "7 3 7 8 15 8" ] []
        ]


svgFeatherIcon : String -> Int -> List (Svg msg) -> Html msg
svgFeatherIcon color s =
    let
        size =
            String.fromInt s
    in
    svg
        [ fill "none"
        , height size
        , stroke color
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width size
        ]
