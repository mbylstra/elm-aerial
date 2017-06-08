module Aerial.Markers exposing (..)

import Aerial.Geo exposing (LatLng)
import Aerial.Model exposing (latLngToViewportPoint)
import Aerial.Types


-- import Aerial.VectorMath exposing (Point2DInt)

import Html exposing (Html, div)
import Html.Attributes exposing (style)


-- import Html.Events exposing (onWithOptions)
-- view : Model -> Aerial.Types.Model -> Html Msg
-- view model aerialModel =
--     let
--         -- _ =
--         --     Debug.log "" True
--         markerWrappers =
--             model.markers
--                 |> List.map
--                 -- |>
--                 --     -- Until we figure out how to pass the model along
--                 --     List.map (always { x = 40, y = 40 })
--                 |>
--                     List.map (markerWrapperView markerView)
--     in
--         div
--             [ class "marker-layer" ]
--             -- []
--             markerWrappers
-- markerWrappers


simpleMarker : Aerial.Types.Model -> LatLng -> Html msg
simpleMarker =
    markerHolder simpleMarkerIcon


markerHolder : Html msg -> Aerial.Types.Model -> LatLng -> Html msg
markerHolder markerIcon aerialModel latLng =
    let
        viewportPosition =
            latLngToViewportPoint aerialModel latLng
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( "left", toString viewportPosition.x ++ "px" )
                , ( "top", toString viewportPosition.y ++ "px" )
                ]
            ]
            [ markerIcon ]


simpleMarkerIcon : Html msg
simpleMarkerIcon =
    div
        [ style
            [ ( "width", "20px" )
            , ( "height", "20px" )
            , ( "position", "absolute" )
            , ( "top", "-10px" )
            , ( "left", "-10px" )
            , ( "border", "1px solid black" )
            , ( "border-radius", "10px" )
            , ( "background-color", "rgba(0,0,0,0.5)" )
            , ( "z-index", "10" )
            ]
        ]
        []
