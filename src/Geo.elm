module Geo exposing (..)

-- import Constants exposing (tileSize, sphericalMercatorEarthRadiusMeters)


type alias LatLng =
    { lat : Float
    , lng : Float
    }


type alias Point2DFloat =
    { x : Float
    , y : Float
    }



-- {-| The bounds are 0.0 - 1.0 in the x and y axis.
--  If you go to the north pole from Alaska you will be near (0.0, 0.0)
--  and if you go to the south pole from New zealand you be near (1.0, 1.0).
-- -}
-- type alias NormalizedSphericalMercatorPoint =
--     Point2DFloat
-- latLngToNormalizedSphericalMercatorPoint : LatLng -> NormalizedSphericalMercatorPoint
-- latLngToNormalizedSphericalMercatorPoint { lat, lng } =
--     Debug.log "merc point" <|
--         { x = (lng + 180.0) / 360.0
--         , y =
--             (1.0
--                 - (((logBase e
--                         (tan
--                             ((pi / 4)
--                                 + ((degrees lat) / 2)
--                             )
--                         )
--                     )
--                         + pi
--                    )
--                     / (2 * pi)
--                   )
--             )
--         }
-- sphericalMercatorPointToWorldPixelPoint : Int -> NormalizedSphericalMercatorPoint -> WorldMapPixelPoint
-- sphericalMercatorPointToWorldPixelPoint zoom { x, y } =
--     let
--         worldMapWidthInTiles =
--             Debug.log "worldMapWidthInTiles" (2 ^ zoom)
--
--         worldMapWidthInPixels =
--             tileSize * worldMapWidthInTiles
--     in
--         Debug.log "world pixel point" <|
--             { x = x * (toFloat worldMapWidthInPixels) |> floor
--             , y = y * (toFloat worldMapWidthInPixels) |> floor
--             , zoom = zoom
--             }
-- worldPixelPointToSphericalMercatorPoint : Int -> WorldMapPixelPoint -> NormalizedSphericalMercatorPoint
-- worldPixelPointToSphericalMercatorPoint zoom { x, y } =
--     let
--         worldMapWidthInTiles =
--             2 ^ zoom
--
--         worldMapWidthInPixels =
--             tileSize * worldMapWidthInTiles
--     in
--         Debug.log "world pixel point" <|
--             { x = (toFloat x) / (toFloat worldMapWidthInPixels)
--             , y = (toFloat y) / (toFloat worldMapWidthInPixels)
--             }
-- worldPixelPointToLatLng : Int -> WorldMapPixelPoint -> LatLng
-- worldPixelPointToLatLng zoom point =
--     point
--         |> worldPixelPointToSphericalMercatorPoint zoom
--         |> sphericalMercatorPointToLatLng
-- sphericalMercatorPointToLatLng : NormalizedSphericalMercatorPoint -> LatLng
-- sphericalMercatorPointToLatLng { x, y } =
--     -- we need to turn y into radians (0 - 1.0 = 180.0 = PI)
--     let
--         yInRadians =
--             (y - 0.5) * pi
--
--         funkyShit =
--             atan (e ^ yInRadians)
--     in
--         { lat = (funkyShit / (pi / 4.0) - 1.0) * 90.0
--         , lng = (x * 360.0) - 180.0
--         }
-- RAD2DEG = 180 / Math.PI;
-- PI_4 = Math.PI / 4;
-- { return (Math.atan(Math.exp (y / RAD2DEG)) / PI_4 - 1) * 90; }
-- { x = (lng + 180.0) / 360.0
-- , y =
--     (1.0
--         - (((logBase e
--                 (tan
--                     ((pi / 4)
--                         + ((degrees lat) / 2)
--                     )
--                 )
--             )
--                 + pi
--            )
--             / (2 * pi)
--           )
--     )
-- }
--         )
-- latLngToWorldPixelPoint : Int -> LatLng -> WorldMapPixelPoint
-- latLngToWorldPixelPoint zoom latLng =
--     latLng
--         |> latLngToNormalizedSphericalMercatorPoint
--         |> sphericalMercatorPointToWorldPixelPoint zoom
-- Another thing to point out is that converting latLng To Meters is a bit pointless,
-- as all we really care about is the value from 0.0 -> 1.0 (we don't really care
-- about how big the earth is). It is useful if we want to measure distances
-- though. Seeing as usually we are going to be converting latlng to screen
-- pixels and vice-versa, there's no real need to convert to meters. If we
-- do we just convert the normalised point (0.0 - 1.0) by (pi * earth radius) which
-- can be pre-calculated
-- This does bring up a vaguely interesting visualisation:
--  we can bring up rings of distances between things (like in that movie Lion)
--
-- Note that usual mercator is lopped off at the top and bottom at 82.0, which
-- makes it look much less rediculous than 90 - 90, which is actually what you
-- see when you zoom right out of google maps tile. So things are actually pretty
-- simple in Google maps.
