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


edit : String -> Int -> Html msg
edit color size =
    svgFeatherIcon color
        size
        [ Svg.path [ d "M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7" ] []
        , Svg.path [ d "M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z" ] []
        ]


thumbsUp : String -> Int -> Html msg
thumbsUp color size =
    svgFeatherIcon color
        size
        [ Svg.path [ d "M14 9V5a3 3 0 0 0-3-3l-4 9v11h11.28a2 2 0 0 0 2-1.7l1.38-9a2 2 0 0 0-2-2.3zM7 22H4a2 2 0 0 1-2-2v-7a2 2 0 0 1 2-2h3" ] []
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
