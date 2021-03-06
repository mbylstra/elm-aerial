module Aerial.SphericalMercator exposing (..)

import Aerial.Util exposing (toRadians, ln, circumference, toDegrees)


{-| in meters on the equator
-}
earthRadiusMeters : Float
earthRadiusMeters =
    6378137


earthCircumferenceMeters : Float
earthCircumferenceMeters =
    -- ~40km
    circumference earthRadiusMeters


lngToProjectedRadians : Float -> Float
lngToProjectedRadians lng =
    toRadians lng


{-| https://wikimedia.org/api/rest_v1/media/math/render/svg/62ea14e55f1e378a2a82a2ff70bee9d2f8cabf8d
-}
latToProjectedRadians : Float -> Float
latToProjectedRadians lat =
    let
        latRadians =
            toRadians lat
    in
        ln
            (tan
                ((pi / 4) + (latRadians / 2))
            )


lngFromProjectedRadians : Float -> Float
lngFromProjectedRadians x =
    toDegrees x


{-| https://wikimedia.org/api/rest_v1/media/math/render/svg/a7abeaed8bf4f766e4eb931035dfbbf787caa6c0
-}
latFromProjectedRadians : Float -> Float
latFromProjectedRadians y =
    let
        latRadians =
            (2 * atan (e ^ y)) - (pi / 2)
    in
        latRadians |> toDegrees


projectLngToMeters : Float -> Float
projectLngToMeters lng =
    projectLngToMapWidth lng earthCircumferenceMeters


projectLatToMeters : Float -> Float
projectLatToMeters lat =
    projectLatToMapWidth lat earthCircumferenceMeters


projectLngToMapWidth : Float -> Float -> Float
projectLngToMapWidth lng width =
    (lngToProjectedRadians lng / pi) * (width / 2)


projectLatToMapWidth : Float -> Float -> Float
projectLatToMapWidth lat width =
    (latToProjectedRadians lat / pi) * (width / 2)


unprojectLngFromMapWidth : Float -> Float -> Float
unprojectLngFromMapWidth width x =
    let
        xRadians =
            -- Debug.log "xRadians" <|
            ((x / (width / 2)) * pi)
    in
        lngFromProjectedRadians xRadians


unprojectLatFromMapWidth : Float -> Float -> Float
unprojectLatFromMapWidth width y =
    latFromProjectedRadians ((y / (width / 2)) * pi)


unprojectLngFromMeters : Float -> Float
unprojectLngFromMeters x =
    unprojectLngFromMapWidth x earthCircumferenceMeters


unprojectLatFromMeters : Float -> Float
unprojectLatFromMeters y =
    unprojectLatFromMapWidth y earthCircumferenceMeters
