module Aerial.MouseEventPositions exposing (..)

import Json.Decode as Decode exposing (field, at, andThen, Decoder)
import Html.Events exposing (on)
import Html exposing (Attribute)


type alias Position =
    { x : Int, y : Int }


{-| see  https://developer.mozilla.org/en-US/docs/Web/API/Event/currentTarget
-}
currentTargetDecoder : Decoder a -> Decoder a
currentTargetDecoder decoder =
    field "currentTarget" decoder


clientXDecoder : Decoder Int
clientXDecoder =
    field "clientX" Decode.int


clientYDecoder : Decoder Int
clientYDecoder =
    field "clientY" Decode.int


offsetLeftDecoder : Decoder Int
offsetLeftDecoder =
    field "offsetLeft" Decode.int


offsetTopDecoder : Decoder Int
offsetTopDecoder =
    field "offsetTop" Decode.int


clientPositionDecoder : Decoder Position
clientPositionDecoder =
    Decode.map2 Position clientXDecoder clientYDecoder


offsetPositionDecoder : Decoder Position
offsetPositionDecoder =
    Decode.map2 Position offsetLeftDecoder offsetTopDecoder


currentTargetOffsetPositionDecoder : Decoder Position
currentTargetOffsetPositionDecoder =
    currentTargetDecoder offsetPositionDecoder


{-| This is really the mouse position relative to the "currentTarget" (the DOM element
    that captured the bubbling event), but this is what most people would want for
   the mouse position, so let's just name it mousePosition
-}
mousePositionDecoder : Decoder Position
mousePositionDecoder =
    Decode.map2
        (\clientPosition currentTarget ->
            { x = clientPosition.x - currentTarget.x
            , y = clientPosition.y - currentTarget.y
            }
        )
        clientPositionDecoder
        currentTargetOffsetPositionDecoder


onMouseMove : Decoder Position -> (Position -> msg) -> Html.Attribute msg
onMouseMove positionDecoder tagger =
    on "mousemove" (Decode.map tagger positionDecoder)


onMouseMoveGetPosition : (Position -> msg) -> Html.Attribute msg
onMouseMoveGetPosition =
    onMouseMove mousePositionDecoder
