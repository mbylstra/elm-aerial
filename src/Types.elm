module Types exposing (..)

import Geo exposing (LatLng)
import Mouse
import VectorMath exposing (Point2DInt, Vector2DInt)
import MouseWheel exposing (MouseWheelEvent)


type alias Model =
    { latLng : LatLng
    , zoom : Int
    , mapWidthPx : Int
    , mapHeightPx : Int
    , resolution :
        Float
        -- 0.5 = half, 1.0 = normal, 2.0 = retina
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
    | MouseLeave Mouse.Position
    | MouseClick Mouse.Position
    | MouseWheel MouseWheelEvent
