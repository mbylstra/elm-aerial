module Model exposing (..)

import Geo exposing (LatLng)
import Types exposing (..)
import VectorMath exposing (Vector2DInt, difference)


initLatLng : LatLng
initLatLng =
    { lat = -37.813611, lng = 144.963056 }


initZoom : Int
initZoom =
    4


model : Model
model =
    { latLng = initLatLng
    , zoom = initZoom
    , mapWidthPx = 1000
    , mapHeightPx = 700
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
