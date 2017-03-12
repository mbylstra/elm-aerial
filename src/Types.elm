module Types exposing (Msg(..), OutMsg(..), Model, MouseOverState)

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



-- which of these should be "public messages"?
-- Some of them are at least a bit pointless, cos you can get it from the model
-- after the update (eg UpdateLat). Also, we have no interest. However,
-- we will want to know if lat lng or zoom has been updated, but we really don't
-- want the raw string.


type Msg parentMsg
    = UpdateLat String
      -- we really have no interest in the raw latlng string. useless.
    | UpdateLng String
    | UpdateZoom String
    | MouseDown Mouse.Position
      -- probably
    | MouseUp
      -- probably
    | MouseMove Mouse.Position
      -- yes
    | MouseEnter Mouse.Position
      -- yes
    | MouseLeave
      -- yes
    | MouseClickEvent Mouse.Position
      -- definitely
    | MouseWheel MouseWheelEvent
    | ParentMsg parentMsg


type OutMsg selfMsg
    = MouseClick Mouse.Position
    | SelfMsg selfMsg
