module Icons exposing (..)

import Html exposing (Html)
import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (..)


check : String -> Int -> Html msg
check color size =
    svgFeatherIcon color
        size
        2
        [ Svg.polyline [ points "20 6 9 17 4 12" ] []
        ]


plus : String -> Int -> Html msg
plus color size =
    svgFeatherIcon color
        size
        3
        [ Svg.line [ x1 "12", y1 "5", x2 "12", y2 "19" ] []
        , Svg.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        ]


plusThin : String -> Int -> Html msg
plusThin color size =
    svgFeatherIcon color
        size
        1
        [ Svg.line [ x1 "12", y1 "5", x2 "12", y2 "19" ] []
        , Svg.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        ]


plusCircle : String -> Int -> Html msg
plusCircle color size =
    svgFeatherIcon color
        size
        2
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12", y2 "16" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
        ]


save : String -> Int -> Html msg
save color size =
    svgFeatherIcon color
        size
        2
        [ Svg.path [ d "M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z" ] []
        , Svg.polyline [ points "17 21 17 13 7 13 7 21" ] []
        , Svg.polyline [ points "7 3 7 8 15 8" ] []
        ]


undo : String -> Int -> Html msg
undo color size =
    svgFeatherIcon color
        size
        2
        [ Svg.polyline [ points "1 4 1 10 7 10" ] []
        , Svg.path [ d "M3.51 15a9 9 0 1 0 2.13-9.36L1 10" ] []
        ]


x : String -> Int -> Html msg
x color size =
    svgFeatherIcon color
        size
        2
        [ Svg.line [ x1 "18", y1 "6", x2 "6", y2 "18" ] []
        , Svg.line [ x1 "6", y1 "6", x2 "18", y2 "18" ] []
        ]


svgFeatherIcon : String -> Int -> Int -> List (Svg msg) -> Html msg
svgFeatherIcon color s sw =
    let
        size =
            String.fromInt s

        strokeW =
            String.fromInt sw
    in
    svg
        [ fill "none"
        , height size
        , stroke color
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth strokeW
        , viewBox "0 0 24 24"
        , width size
        ]
