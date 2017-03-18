module Aerial.MouseWheel exposing (..)

import Json.Decode as Decode exposing (field)
import Html.Events exposing (onWithOptions)
import Html


type alias MouseWheelEvent =
    { deltaX : Float
    , deltaY : Float
    , deltaZ : Float
    , deltaMode : MouseWheelDeltaMode
    }


type MouseWheelDeltaMode
    = DeltaModePixel
    | DeltaModeLine
    | DeltaModePage
    | DeltaModeUnknown


onMouseWheel : (MouseWheelEvent -> msg) -> Html.Attribute msg
onMouseWheel tagger =
    onWithOptions "wheel"
        { preventDefault = True
        , stopPropagation = True
        }
        (Decode.map tagger mouseWheelEventDecoder)


mouseWheelEventDecoder : Decode.Decoder MouseWheelEvent
mouseWheelEventDecoder =
    Decode.map4
        MouseWheelEvent
        (field "deltaX" Decode.float)
        (field "deltaY" Decode.float)
        (field "deltaZ" Decode.float)
        (field "deltaMode" Decode.int |> Decode.map decodeDeltaMode)


decodeDeltaMode : Int -> MouseWheelDeltaMode
decodeDeltaMode rawMode =
    case rawMode of
        0 ->
            DeltaModePixel

        1 ->
            DeltaModeLine

        2 ->
            DeltaModePage

        _ ->
            DeltaModeUnknown
