module Model exposing (..)

import Geo exposing (LatLng)
import SlippyTiles exposing (WorldMapPixelPoint, getTileTopLeftWorldPixelPoint, latLngToWorldPixelPoint, worldPixelPointToLatLng, worldPixelPointToSlippyTileNumber)
import Tiles
import Types exposing (..)
import Util exposing (fmod)
import VectorMath exposing (Point2DInt, Vector2DInt, difference, scalarMultiply, timesFloatToInt)


initLatLng : LatLng
initLatLng =
    { lat = -37.813611, lng = 144.963056 }



-- { lat = 0.0, lng = 0.0 }


initZoom : Int
initZoom =
    2


init : Model
init =
    { latLng = initLatLng
    , zoom =
        initZoom
        -- , mapWidthPx = 512
        -- , mapHeightPx =
        --     512
    , mapWidthPx = 1000
    , mapHeightPx =
        800
        -- , mapWidthPx = 256
        -- , mapHeightPx = 256
        -- , resolution = 0.25
    , resolution = 1.0
    , maybeMouseOver = Nothing
    }


getDraggingOffset : MouseOverState -> Maybe Vector2DInt
getDraggingOffset mouseOverState =
    mouseOverState.down
        |> Maybe.map
            (.startPosition
                >> difference mouseOverState.position
                >> VectorMath.negate
            )


viewportPointToWorldPixelPoint : Point2DInt -> Model -> WorldMapPixelPoint
viewportPointToWorldPixelPoint viewportPoint model =
    let
        viewportTopLeftWorldPixel =
            -- Debug.log "viewportTopLeftWorldPixel" <|
            getViewportTopLeftWorldPixel model model.zoom 1.0
    in
        { x = viewportTopLeftWorldPixel.x + viewportPoint.x
        , y = viewportTopLeftWorldPixel.y + viewportPoint.y
        , zoom = model.zoom
        }


viewportPointToLatLng : Model -> Point2DInt -> LatLng
viewportPointToLatLng model viewportPoint =
    -- (Debug.log "clicked worldPixelPoint" <|
    viewportPointToWorldPixelPoint viewportPoint model
        -- )
        |>
            worldPixelPointToLatLng



-- latLngToWorldPixelPoint : Int -> LatLng -> WorldMapPixelPoint
-- latLngToWorldPixelPoint zoom latLng =
--     latLngToWorldPixelPoint zoom latLng


getViewportCenter : Model -> Point2DInt
getViewportCenter model =
    { x = model.mapWidthPx // 2
    , y = model.mapHeightPx // 2
    }


{-| This is the width in the tiles at 1:1 resolution.
-}
getWorldWidthInTiles : Model -> Int
getWorldWidthInTiles model =
    Tiles.getWorldWidthInTiles model.zoom


{-| this is the same if the resolution is zero
-}
mapWidthInWorldPixels : Int -> Float -> Int
mapWidthInWorldPixels mapWidthPx resolution =
    Debug.log "mapWidthInWorldPixels" <|
        timesFloatToInt mapWidthPx resolution


mapHeightInWorldPixels : Int -> Float -> Int
mapHeightInWorldPixels mapHeightPx resolution =
    Debug.log "mapHeightInWorldPixels" <|
        timesFloatToInt mapHeightPx resolution


getViewportTopLeftWorldPixel : Model -> Int -> Float -> WorldMapPixelPoint
getViewportTopLeftWorldPixel model zoom resolution =
    -- I mean, this totally depends on the resolution :(
    -- I think there's an issue here.
    -- Cos we are confusing the WorldPixel where we use atual pixel width
    -- vs the pretendWorldPixel ???
    -- I think the trick is to measure the "mapWidth" in terms of worldPixels, not screen pixels
    let
        -- this is correct
        mapCenter =
            Debug.log "mapCenterWorldPixel" <|
                latLngToWorldPixelPoint zoom model.latLng
    in
        -- but this doesn't take into account resolution
        -- { x = (timesFloatToInt mapCenter.x resolution) - ((timesFloatToInt model.mapWidthPx resolution) // 2)
        -- , y = (timesFloatToInt mapCenter.y resolution) - ((timesFloatToInt model.mapHeightPx resolution) // 2)
        { x = mapCenter.x - ((mapWidthInWorldPixels model.mapWidthPx resolution) // 2)
        , y =
            mapCenter.y - ((mapHeightInWorldPixels model.mapHeightPx resolution) // 2)
            -- { x = mapCenter.x - (model.mapWidthPx // 2)
            -- , y = mapCenter.y - (model.mapHeightPx // 2)
        , zoom = zoom
        }


getViewportBottomRightWorldPixel : Model -> Int -> Float -> WorldMapPixelPoint
getViewportBottomRightWorldPixel model zoom resolution =
    let
        mapCenter =
            latLngToWorldPixelPoint zoom model.latLng
    in
        { x = mapCenter.x + ((mapWidthInWorldPixels model.mapWidthPx resolution) // 2)
        , y = mapCenter.y + ((mapHeightInWorldPixels model.mapHeightPx resolution) // 2)
        , zoom = zoom
        }


worldPixelPointToViewportPoint : WorldMapPixelPoint -> Model -> Int -> Float -> Point2DInt
worldPixelPointToViewportPoint worldPixelPoint model zoom resolution =
    let
        _ =
            Debug.log "worldPixelPoint B" worldPixelPoint

        viewportTopLeftWorldPixel =
            Debug.log "viewportTopLeftWorldPixel B" <|
                getViewportTopLeftWorldPixel model zoom resolution

        vector =
            { x = worldPixelPoint.x - viewportTopLeftWorldPixel.x
            , y = worldPixelPoint.y - viewportTopLeftWorldPixel.y
            }
    in
        -- These need to get scaled up by resolution ro something?
        VectorMath.scalarMultiplyByFloat vector (1.0 / resolution)


getViewportCenterOffset : Model -> Point2DInt -> Vector2DInt
getViewportCenterOffset model viewportPoint =
    VectorMath.difference (getViewportCenter model) viewportPoint


zoomAtCursor : Model -> Bool -> Model
zoomAtCursor model zoomIn =
    case model.maybeMouseOver of
        Just mouseOverState ->
            let
                _ =
                    Debug.log "model" model

                zoomDelta =
                    Debug.log "zoomDelta" <|
                        if zoomIn then
                            1
                        else
                            -1

                -- VectorMath.scalarMultiply viewportCenterOffset -1
                -- (2 ^ zoomDelta)
                newModel =
                    { model | zoom = cleanZoom (model.zoom + zoomDelta) }

                newZoom =
                    newModel.zoom
            in
                if newZoom /= model.zoom then
                    let
                        viewportCenterOffset =
                            Debug.log "viewportcenteroffset" <|
                                getViewportCenterOffset model mouseOverState.position

                        panVector =
                            -- To be honest I don't know why the math is different for zooming in vs zooming out
                            -- First I got zoom out working, then I hacked around at things until zoom in
                            -- worked, and now they both work!
                            Debug.log "panVector" <|
                                if zoomIn then
                                    VectorMath.negate viewportCenterOffset
                                else
                                    VectorMath.scalarMultiply viewportCenterOffset (2 ^ zoomDelta)
                    in
                        Debug.log "model3" <|
                            panByPixels newModel panVector
                else
                    model

        -- model2
        Nothing ->
            model


panByPixels : Model -> Vector2DInt -> Model
panByPixels model vector =
    -- get latlng at mapCenter - vector
    let
        mapCenter =
            latLngToWorldPixelPoint model.zoom model.latLng

        newMapCenter =
            VectorMath.minusVector { x = mapCenter.x, y = mapCenter.y } vector

        newLatLng =
            worldPixelPointToLatLng { x = newMapCenter.x, y = newMapCenter.y, zoom = model.zoom }
    in
        { model | latLng = newLatLng }


cleanZoom : Int -> Int
cleanZoom zoom =
    if zoom > 19 then
        19
    else if zoom < 2 then
        2
    else
        zoom


setZoom : Model -> Int -> Model
setZoom model zoom =
    { model | zoom = zoom }


cleanLatLng : LatLng -> LatLng
cleanLatLng { lat, lng } =
    { lat = cleanLat lat
    , lng = cleanLng lng
    }


cleanLng : Float -> Float
cleanLng lng =
    (fmod (lng + 180.0) 360.0)
        + -180.0


cleanLat : Float -> Float
cleanLat lat =
    if lat >= 90.0 then
        90.0
    else if lat <= -90.0 then
        -90.0
    else
        lat
