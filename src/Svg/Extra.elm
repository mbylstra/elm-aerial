module Svg.Extra exposing (..)

import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, r)


centeredCircle : Int -> Int -> Int -> List (Svg.Attribute msg) -> Svg msg
centeredCircle x y radius attrs =
    let
        topLeftX =
            toString (x - radius)

        topLeftY =
            toString (y - radius)
    in
        Svg.circle ([ cx topLeftX, cy topLeftY, r <| toString radius ] ++ attrs) []


circle : Int -> Int -> Int -> List (Svg.Attribute msg) -> Svg msg
circle x y radius attrs =
    let
        topLeftX =
            toString x

        topLeftY =
            toString y
    in
        Svg.circle ([ cx topLeftX, cy topLeftY, r <| toString radius ] ++ attrs) []
