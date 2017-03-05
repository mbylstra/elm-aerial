module Types exposing (..)

import Geo exposing (LatLng)
import Mouse
import SlippyTiles exposing (SlippyTileNumber)
import VectorMath exposing (Point2DInt, Vector2DInt)
import MouseWheel exposing (MouseWheelEvent)


type alias Model =
    { latLng : LatLng
    , zoom : Int
    , mapWidthPx : Int
    , mapHeightPx : Int
    , maybeMouseOver : Maybe MouseOverState
    }


type alias MouseOverState =
    { position : Point2DInt, down : Maybe { startPosition : Vector2DInt } }


type Msg
    = UpdateLat String
    | UpdateLng String
    | UpdateZoom String
    | MouseDown Mouse.Position
    | MouseUp
    | MouseMove Mouse.Position
    | MouseEnter Mouse.Position
    | MouseLeave
    | MouseClick Mouse.Position
    | MouseWheel MouseWheelEvent


type alias TileViewModel =
    { tileNumber : SlippyTileNumber, viewportPoint : Point2DInt }
