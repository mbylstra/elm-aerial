module Geo exposing (..)

import Constants exposing (slippyTileSize, sphericalMercatorEarthRadiusMeters)


type alias LatLng =
    { lat : Float
    , lng : Float
    }


type alias Point2DFloat =
    { x : Float
    , y : Float
    }


type alias Point2DInt =
    { x : Int
    , y : Int
    }


{-| The bounds are 0.0 - 1.0 in the x and y axis.
 If you go to the north pole from Alaska you will be near (0.0, 0.0)
 and if you go to the south pole from New zealand you be near (1.0, 1.0).
-}
type alias NormalizedSphericalMercatorPoint =
    Point2DFloat


type alias WorldMapPixelPoint =
    { x : Int
    , y : Int
    , zoom : Int
    }


type alias SlippyTileNumber =
    { x : Int
    , y : Int
    , zoom : Int
    }


latLngToNormalizedSphericalMercatorPoint : LatLng -> NormalizedSphericalMercatorPoint
latLngToNormalizedSphericalMercatorPoint { lat, lng } =
    Debug.log "merc point" <|
        { x = (lng + 180.0) / 360.0
        , y =
            (1.0
                - (((logBase e
                        (tan
                            ((pi / 4)
                                + ((degrees lat) / 2)
                            )
                        )
                    )
                        + pi
                   )
                    / (2 * pi)
                  )
            )
        }


sphericalMercatorPointToWorldPixelPoint : Int -> NormalizedSphericalMercatorPoint -> WorldMapPixelPoint
sphericalMercatorPointToWorldPixelPoint zoom { x, y } =
    let
        worldMapWidthInTiles =
            Debug.log "worldMapWidthInTiles" (2 ^ zoom)

        worldMapHeightIntTiles =
            worldMapWidthInTiles

        worldMapWidthInPixels =
            slippyTileSize * worldMapWidthInTiles

        worldMapHeightInPixels =
            Debug.log "worldMapWidthInPixels" worldMapWidthInPixels
    in
        Debug.log "world pixel point" <|
            { x = x * (toFloat worldMapWidthInPixels) |> floor
            , y = y * (toFloat worldMapHeightInPixels) |> floor
            , zoom = zoom
            }


worldPixelPointToSlippyTileNumber : WorldMapPixelPoint -> SlippyTileNumber
worldPixelPointToSlippyTileNumber { x, y, zoom } =
    { x = x // slippyTileSize
    , y = y // slippyTileSize
    , zoom = zoom
    }


latLngToSlippyTileNumber : Int -> LatLng -> SlippyTileNumber
latLngToSlippyTileNumber zoom latLng =
    Debug.log "slippyTileNumber"
        (latLng
            |> latLngToNormalizedSphericalMercatorPoint
            |> sphericalMercatorPointToWorldPixelPoint zoom
            |> worldPixelPointToSlippyTileNumber
        )


latLngToWorldPixelPoint : Int -> LatLng -> WorldMapPixelPoint
latLngToWorldPixelPoint zoom latLng =
    latLng
        |> latLngToNormalizedSphericalMercatorPoint
        |> sphericalMercatorPointToWorldPixelPoint zoom


getTileTopLeftWorldPixelPoint : SlippyTileNumber -> WorldMapPixelPoint
getTileTopLeftWorldPixelPoint slippyTileNumber =
    { x = slippyTileNumber.x * slippyTileSize
    , y = slippyTileNumber.y * slippyTileSize
    , zoom = slippyTileNumber.zoom
    }


slippyTileUrl : String -> SlippyTileNumber -> String
slippyTileUrl subdomain { x, y, zoom } =
    let
        ( xS, yS, zS ) =
            ( toString x, toString y, toString zoom )
    in
        "http://" ++ subdomain ++ ".tile.osm.org/" ++ zS ++ "/" ++ xS ++ "/" ++ yS ++ ".png"



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
