module Aerial.SlippyTiles.Tests exposing (..)

import Test exposing (..)
import Expect
import SlippyTiles
    exposing
        ( latLngToWorldPixelPoint
        , tileSize
        )
import Geo exposing (LatLng)


melbourneLatLng : LatLng
melbourneLatLng =
    { lat = -37.813611, lng = 144.963056 }


alertCanadaLatLng : LatLng
alertCanadaLatLng =
    { lat = 82.5018, lng = 62.3481 }


million : Float
million =
    1000000.0


all : Test
all =
    describe "SlippyTiles.elm"
        [ describe "latLngToWorldPixelPoint"
            [ test "from lat 0.0 lng 0.0 zoom 0" <|
                \() ->
                    latLngToWorldPixelPoint 0 { lat = 0.0, lng = 0.0 }
                        |> Expect.equal { x = tileSize // 2, y = tileSize // 2, zoom = 0 }
            , test "from lat 0.0 lng 90.0 zoom 0" <|
                \() ->
                    latLngToWorldPixelPoint 0 { lat = 0.0, lng = -90.0 }
                        |> Expect.equal { x = tileSize // 4, y = tileSize // 2, zoom = 0 }
            , test "from lat 45.0 lng 0.0 zoom 0" <|
                \() ->
                    latLngToWorldPixelPoint 0 { lat = 45.0, lng = -180.0 }
                        |> Expect.equal { x = 0, y = 92, zoom = 0 }
            , test "from Alert Canada zoom 0" <|
                \() ->
                    let
                        point =
                            latLngToWorldPixelPoint 0 alertCanadaLatLng
                    in
                        point.y
                            |> Expect.atLeast 0
            ]
        ]
