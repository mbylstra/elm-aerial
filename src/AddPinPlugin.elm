module AddPinPlugin exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Notice that there are no requirements from Demo.elm. This is important for it to be a plugin!

import Geo exposing (LatLng)
import Json.Decode as Json
import Model as AerialModel exposing (latLngToViewportPoint, viewportPointToLatLng)
import Types as AerialTypes


-- import Update as AerialUpdate

import VectorMath exposing (Point2DInt)


-- import View as AerialView


type alias Model =
    { markers : List LatLng
    }


init : Model
init =
    { markers = [ LatLng -37.814 144.96332, LatLng -33.86785 151.20732 ]
    }


type Msg
    = MarkerMouseClick


update : Msg -> Model -> AerialTypes.Model -> Model
update msg model aerialModel =
    case msg of
        MarkerMouseClick ->
            let
                _ =
                    Debug.log "AddPinPlugin MarkerMouseClick" True
            in
                model


updateWithAerialOutMsg : AerialTypes.OutMsg -> AerialTypes.Model -> Model -> Model
updateWithAerialOutMsg outMsg aerialModel model =
    case outMsg of
        AerialTypes.MouseClick position ->
            -- this just gives the position, but we can give position + aerialModel
            -- to get the latlng
            -- then we add it to the list of lat lngs.. pretty easy!
            let
                latLng =
                    viewportPointToLatLng aerialModel position

                newMarkers =
                    model.markers ++ [ latLng ]
            in
                { model | markers = newMarkers }



--
--             Nothing ->
--                 model2 ! []
-- view : Model -> AerialTypes.Model -> Html Msg


view : Model -> AerialTypes.Model -> Html Msg
view model aerialModel =
    let
        -- _ =
        --     Debug.log "" True
        markerWrappers =
            model.markers
                |> List.map (latLngToViewportPoint aerialModel)
                -- |>
                --     -- Until we figure out how to pass the model along
                --     List.map (always { x = 40, y = 40 })
                |>
                    List.map (markerWrapperView markerView)
    in
        div
            [ class "marker-layer" ]
            -- []
            markerWrappers



-- markerWrappers


markerWrapperView : Html Msg -> Point2DInt -> Html Msg
markerWrapperView markerView viewportPosition =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", toString viewportPosition.x ++ "px" )
            , ( "top", toString viewportPosition.y ++ "px" )
            ]
        ]
        [ markerView ]


markerView : Html Msg
markerView =
    div
        [ style
            [ ( "width", "20px" )
            , ( "height", "20px" )
            , ( "position", "absolute" )
            , ( "top", "-10px" )
            , ( "left", "-10px" )
            , ( "border", "1px solid black" )
            , ( "border-radius", "10px" )
            , ( "background-color", "rgba(0,0,0,0.5)" )
            , ( "z-index", "10" )
            ]
        , onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.succeed MarkerMouseClick)
        ]
        []
