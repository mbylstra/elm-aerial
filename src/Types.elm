module Types exposing (..)

import Mouse
import Geo exposing (LatLng)
import VectorMath exposing (Point2DInt, Vector2DInt)


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
