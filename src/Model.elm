module Model exposing (..)

import Geo exposing (LatLng)
import SlippyTiles exposing (WorldMapPixelPoint, getTileTopLeftWorldPixelPoint, latLngToWorldPixelPoint, tileSize, worldPixelPointToLatLng, worldPixelPointToSlippyTileNumber)
import Types exposing (..)
import Util exposing (cartesianProduct)
import VectorMath exposing (Point2DInt, Vector2DInt, difference)


initLatLng : LatLng
initLatLng =
    { lat = -37.813611, lng = 144.963056 }


initZoom : Int
initZoom =
    4


model : Model
model =
    { latLng = initLatLng
    , zoom =
        initZoom
    , mapWidthPx = 1000
    , mapHeightPx =
        700
        -- , mapWidthPx = 256
        -- , mapHeightPx = 256
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
            getViewportTopLeftWorldPixel model
    in
        { x = viewportTopLeftWorldPixel.x + viewportPoint.x
        , y = viewportTopLeftWorldPixel.y + viewportPoint.y
        , zoom = model.zoom
        }


viewportPointToLatLng : Point2DInt -> Model -> LatLng
viewportPointToLatLng viewportPoint model =
    -- (Debug.log "clicked worldPixelPoint" <|
    viewportPointToWorldPixelPoint viewportPoint model
        -- )
        |>
            worldPixelPointToLatLng


getMapCenterAsWorldPixelPoint : Model -> WorldMapPixelPoint
getMapCenterAsWorldPixelPoint model =
    latLngToWorldPixelPoint model.zoom model.latLng


getWorldWidthInTiles : Model -> Int
getWorldWidthInTiles model =
    2 ^ model.zoom


getViewportTopLeftWorldPixel : Model -> WorldMapPixelPoint
getViewportTopLeftWorldPixel model =
    let
        mapCenter =
            -- Debug.log "mapCenterWorldPixel" <|
            getMapCenterAsWorldPixelPoint model
    in
        { x = mapCenter.x - (model.mapWidthPx // 2)
        , y = mapCenter.y - (model.mapHeightPx // 2)
        , zoom = model.zoom
        }


getViewportBottomRightWorldPixel : Model -> WorldMapPixelPoint
getViewportBottomRightWorldPixel model =
    let
        mapCenter =
            getMapCenterAsWorldPixelPoint model
    in
        { x = mapCenter.x + (model.mapWidthPx // 2)
        , y = mapCenter.y + (model.mapHeightPx // 2)
        , zoom = model.zoom
        }


worldPixelPointToViewportPoint : WorldMapPixelPoint -> Model -> Point2DInt
worldPixelPointToViewportPoint worldPixelPoint model =
    let
        viewportTopLeftWorldPixel =
            getViewportTopLeftWorldPixel model
    in
        { x = worldPixelPoint.x - viewportTopLeftWorldPixel.x
        , y = worldPixelPoint.y - viewportTopLeftWorldPixel.y
        }


getTileViewModels : Model -> List TileViewModel
getTileViewModels model =
    let
        worldWidthInTiles =
            getWorldWidthInTiles model

        viewportTopLeftWorldPixel =
            getViewportTopLeftWorldPixel model

        viewportBottomRightWorldPixel =
            getViewportBottomRightWorldPixel model

        topLeftTileNumber =
            worldPixelPointToSlippyTileNumber viewportTopLeftWorldPixel

        topLeftTileWorldPixelPoint =
            getTileTopLeftWorldPixelPoint topLeftTileNumber

        topLeftTileViewportPoint =
            worldPixelPointToViewportPoint topLeftTileWorldPixelPoint model

        numColumns =
            -- Debug.log "numColumns" <|
            (viewportBottomRightWorldPixel.x - topLeftTileWorldPixelPoint.x)
                // tileSize
                + 1

        numRows =
            -- Debug.log "numRows" <|
            (viewportBottomRightWorldPixel.y - topLeftTileWorldPixelPoint.y)
                // tileSize
                + 1

        matrix : List ( Int, Int )
        matrix =
            -- Debug.log "matrix" <|
            cartesianProduct (List.range 0 (numColumns - 1)) (List.range 0 (numRows - 1))

        toTileViewModel ( viewportTileX, viewportTileY ) =
            let
                -- we really want to use translation functions here? Maybe use opensolid/geometry?
                currTileTopLeftViewportPoint =
                    { x = topLeftTileViewportPoint.x + (viewportTileX * tileSize)
                    , y = topLeftTileViewportPoint.y + (viewportTileY * tileSize)
                    }

                currTileBottomRightViewportPoint =
                    { x = currTileTopLeftViewportPoint.x + tileSize
                    , y = currTileTopLeftViewportPoint.y + tileSize
                    }
            in
                { tileNumber =
                    { x = (topLeftTileNumber.x + viewportTileX) % worldWidthInTiles
                    , y = (topLeftTileNumber.y + viewportTileY) % worldWidthInTiles
                    , zoom = topLeftTileNumber.zoom
                    }
                , viewportPoint = currTileTopLeftViewportPoint
                }

        -- we shouldn't keep zoom around for each tile (no need!) - it just needs to be set right at the top
    in
        List.map toTileViewModel matrix
