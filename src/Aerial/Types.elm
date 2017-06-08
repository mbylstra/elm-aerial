module Aerial.Types exposing (Msg(..), OutMsg(..), Model, MouseOverState, Return(..))

import Mouse
import Aerial.Geo exposing (LatLng)
import Aerial.VectorMath exposing (Point2DInt, Vector2DInt)
import Aerial.MouseWheel exposing (MouseWheelEvent)


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
    = ParentMsg parentMsg
    | UpdateLat String
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
    | PrivateMouseClick Mouse.Position
      -- definitely
    | MouseLeave Mouse.Position
    | MouseWheel MouseWheelEvent


type Return parentMsg
    = SelfMsg parentMsg
    | OutMsg OutMsg
    | ReturnNothing


type OutMsg
    = MouseClick Mouse.Position
