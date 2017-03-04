module Tests exposing (..)

import Test exposing (..)


-- import Expect
-- import Geo
--     exposing
--         ( worldPixelPointToSphericalMercatorPoint
--         , latLngToWorldPixelPoint
--         , worldPixelPointToLatLng
--         , sphericalMercatorPointToLatLng
--         )

import SphericalMercator.Tests
import SlippyTiles.Tests


all : Test
all =
    describe "Main Test Suite"
        [ SphericalMercator.Tests.all
        , SlippyTiles.Tests.all
          -- , describe "Geo.elm"
          --     [ test "latLngToWorldPixelPoint, centered, zoom = 0" <|
          --         \() ->
          --             latLngToWorldPixelPoint 0 { lat = 0.0, lng = 0.0 }
          --                 |> Expect.equal { zoom = 0, x = 128, y = 128 }
          --     , test "latLngToWorldPixelPoint, 45degrees, zoom = 0" <|
          --         \() ->
          --             latLngToWorldPixelPoint 0 { lat = 45.0, lng = 45.0 }
          --                 |> Expect.equal { zoom = 0, x = 160, y = 92 }
          --     , test "pixellatLngToWorldPixelPoint, centered, zoom = 0" <|
          --         \() ->
          --             worldPixelPointToLatLng 0 { x = 128, y = 128, zoom = 0 }
          --                 |> Expect.equal { lat = 0.0, lng = 0.0 }
          --     , test "pixellatLngToWorldPixelPoint, 45degrees, zoom = 0" <|
          --         \() ->
          --             worldPixelPointToLatLng 0 { x = 160, y = 92, zoom = 0 }
          --                 |> Expect.equal { lat = 45.0, lng = 45.0 }
          --     , test "worldPixelPointToSphericalMercatorPoint" <|
          --         \() ->
          --             worldPixelPointToSphericalMercatorPoint 0 { x = 128, y = 128, zoom = 0 }
          --                 |> Expect.equal { x = 0.5, y = 0.5 }
          --     , test "worldPixelPointToSphericalMercatorPoint" <|
          --         \() ->
          --             worldPixelPointToSphericalMercatorPoint 0 { x = 160, y = 92, zoom = 0 }
          --                 |> Expect.equal { x = 0.625, y = 0.359375 }
          --     , test "sphericalMercatorPointToLatLng" <|
          --         \() ->
          --             sphericalMercatorPointToLatLng { x = 0.0, y = 0.5 }
          --                 |> Expect.equal { lat = 0.0, lng = -180.0 }
          --     , test "sphericalMercatorPointToLatLng" <|
          --         \() ->
          --             sphericalMercatorPointToLatLng { x = 0.0, y = 0.25 }
          --                 |> Expect.equal { lat = 45.0, lng = -180.0 }
          --     ]
        ]
