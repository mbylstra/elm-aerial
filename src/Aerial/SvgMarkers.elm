module Aerial.SvgMarkers exposing (..)

import Aerial.Geo exposing (LatLng)
import Aerial.Model exposing (latLngToViewportPoint)
import Aerial.Types
import Svg exposing (Svg, g)
import Svg.Attributes exposing (transform)


translate : Int -> Int -> String
translate x y =
    "translate(" ++ toString x ++ "," ++ toString y ++ ")"


svgMarkerHolder : Svg msg -> Aerial.Types.Model -> LatLng -> Svg msg
svgMarkerHolder markerIcon aerialModel latLng =
    let
        viewportPosition =
            latLngToViewportPoint aerialModel latLng
    in
        g
            [ transform <| translate viewportPosition.x viewportPosition.y ]
            [ markerIcon ]
