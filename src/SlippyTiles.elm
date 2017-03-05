module SlippyTiles exposing (..)

import Geo exposing (LatLng)
import SphericalMercator
    exposing
        ( projectLatToMapWidth
        , projectLngToMapWidth
        , unprojectLngFromMapWidth
        , unprojectLatFromMapWidth
        )


tileSize : Int
tileSize =
    256


type alias SlippyTileNumber =
    { x : Int
    , y : Int
    , zoom : Int
    }


type alias WorldMapPixelPoint =
    { x : Int
    , y : Int
    , zoom : Int
    }


getTileTopLeftWorldPixelPoint : SlippyTileNumber -> WorldMapPixelPoint
getTileTopLeftWorldPixelPoint slippyTileNumber =
    { x = slippyTileNumber.x * tileSize
    , y = slippyTileNumber.y * tileSize
    , zoom = slippyTileNumber.zoom
    }


worldPixelPointToSlippyTileNumber : WorldMapPixelPoint -> SlippyTileNumber
worldPixelPointToSlippyTileNumber { x, y, zoom } =
    { x = x // tileSize
    , y = y // tileSize
    , zoom = zoom
    }


slippyTileUrl : String -> SlippyTileNumber -> String
slippyTileUrl subdomain { x, y, zoom } =
    let
        ( xS, yS, zS ) =
            ( toString x, toString y, toString zoom )
    in
        "http://" ++ subdomain ++ ".tile.osm.org/" ++ zS ++ "/" ++ xS ++ "/" ++ yS ++ ".png"


getMapWidth : Int -> Float
getMapWidth zoom =
    (2 ^ zoom)
        * tileSize
        |> toFloat


latLngToWorldPixelPoint : Int -> LatLng -> WorldMapPixelPoint
latLngToWorldPixelPoint zoom { lat, lng } =
    let
        -- the width of the entire map in pixels
        mapWidth =
            getMapWidth zoom

        halfMapWidth =
            mapWidth / 2

        x =
            ((projectLngToMapWidth lng mapWidth)
                + halfMapWidth
            )
                |> floor

        y =
            (-(projectLatToMapWidth lat mapWidth)
                + halfMapWidth
            )
                |> floor
    in
        { x = x, y = y, zoom = zoom }


latLngToSlippyTileNumber : Int -> LatLng -> SlippyTileNumber
latLngToSlippyTileNumber zoom latLng =
    -- Debug.log "slippyTileNumber" <|
    (latLng
        |> latLngToWorldPixelPoint zoom
        |> worldPixelPointToSlippyTileNumber
    )



-- This is broken


worldPixelPointToLatLng : WorldMapPixelPoint -> LatLng
worldPixelPointToLatLng { zoom, x, y } =
    let
        mapWidth =
            -- Debug.log "mapWidth" <|
            getMapWidth zoom

        halfMapWidth =
            mapWidth / 2

        mercatorX =
            -- Debug.log "mercatorX" <|
            (toFloat x)
                - halfMapWidth

        mercatorY =
            -- Debug.log "mercatorY" <|
            (mapWidth - (toFloat y))
                - halfMapWidth

        lng =
            unprojectLngFromMapWidth mapWidth mercatorX

        lat =
            unprojectLatFromMapWidth mapWidth mercatorY
    in
        { lat = lat
        , lng = lng
        }
