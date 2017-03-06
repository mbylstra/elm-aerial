module ViewModel exposing (..)

import Model exposing (getViewportBottomRightWorldPixel, getViewportTopLeftWorldPixel, getWorldWidthInTiles, worldPixelPointToViewportPoint)
import SlippyTiles exposing (SlippyTileNumber, getTileTopLeftWorldPixelPoint, worldPixelPointToSlippyTileNumber)
import Tiles exposing (getTileSize, getTileViewZoom)
import Types exposing (..)
import Util exposing (cartesianProduct)
import VectorMath exposing (Point2DInt, timesFloatToInt)


type alias TileViewModel =
    { tileNumber : SlippyTileNumber
    , viewportPoint : Point2DInt
    , tileSize : Int
    }


getTileViewModels : Model -> Float -> List TileViewModel
getTileViewModels model resolution =
    let
        zoom =
            Debug.log "tileViewZoom" <|
                getTileViewZoom model.zoom resolution

        worldWidthInTiles =
            Debug.log "tileView worldWidthInTiles" <|
                Tiles.getWorldWidthInTiles zoom

        viewportTopLeftWorldPixel =
            Debug.log "vieportTopLeftWOrldPixle" <|
                getViewportTopLeftWorldPixel model zoom resolution

        viewportBottomRightWorldPixel =
            Debug.log "vieportBottomRightLeftWorldPixel" <|
                getViewportBottomRightWorldPixel model zoom resolution

        -- This is where we want to take current resolution into account
        -- also rember that the zoom number is smaller or greater depending
        -- on the zoom amount
        topLeftTileNumber =
            Debug.log "topLeftTileNumber" <|
                worldPixelPointToSlippyTileNumber viewportTopLeftWorldPixel

        topLeftTileWorldPixelPoint =
            Debug.log "topLeftTileWorldPixelPoint" <|
                getTileTopLeftWorldPixelPoint topLeftTileNumber

        topLeftTileViewportPoint =
            Debug.log "topLeftTileViewportPoint" <|
                worldPixelPointToViewportPoint topLeftTileWorldPixelPoint model zoom resolution

        tileSize =
            Debug.log "tileView tileSize" <|
                getTileSize resolution

        -- numColumns =
        --     -- this needs to take into account resolution
        --     Debug.log "numColumns" <|
        --         (timesFloatToInt
        --             ((viewportBottomRightWorldPixel.x - topLeftTileWorldPixelPoint.x)
        --                 // tileSize
        --             )
        --             (1.0 / resolution)
        --             + 1
        --         )
        numColumns =
            Debug.log "numColumns" <|
                ((timesFloatToInt (viewportBottomRightWorldPixel.x - topLeftTileWorldPixelPoint.x) (1.0 / resolution))
                    // tileSize
                )
                    + 1

        numRows =
            Debug.log "numColumns" <|
                ((timesFloatToInt (viewportBottomRightWorldPixel.y - topLeftTileWorldPixelPoint.y) (1.0 / resolution))
                    // tileSize
                )
                    + 1

        matrix : List ( Int, Int )
        matrix =
            -- Debug.log "matrix" <|
            cartesianProduct (List.range 0 (numColumns - 1)) (List.range 0 (numRows - 1))

        argsRecord =
            { topLeftTileViewportPoint = topLeftTileViewportPoint
            , topLeftTileNumber = topLeftTileNumber
            , tileSize = tileSize
            , worldWidthInTiles = worldWidthInTiles
            , zoom = zoom
            }
    in
        List.map (toTileViewModel argsRecord) matrix


toTileViewModel :
    { topLeftTileViewportPoint : Point2DInt
    , topLeftTileNumber : SlippyTileNumber
    , tileSize : Int
    , worldWidthInTiles : Int
    , zoom : Int
    }
    -> ( Int, Int )
    -> TileViewModel
toTileViewModel { topLeftTileViewportPoint, topLeftTileNumber, tileSize, worldWidthInTiles, zoom } ( viewportTileX, viewportTileY ) =
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
            , zoom = zoom
            }
        , viewportPoint = currTileTopLeftViewportPoint
        , tileSize = tileSize
        }


getTileViewTileSize : Float -> Int
getTileViewTileSize resolution =
    (toFloat SlippyTiles.tileSize) / resolution |> floor
