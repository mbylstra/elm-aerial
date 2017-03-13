module AddPinPlugin exposing (..)

import Geo exposing (LatLng)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


-- import Model as AerialModel exposing (latLngToViewportPoint, viewportPointToLatLng)
-- import Types as AerialTypes
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
    = MouseClick



--     | ???


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- AerialMsg aerialMsg ->
        --     let
        --         -- Here we might get an AerialMsg or one of our own messages.
        --         --We *could* just return all messages, but we probably
        --         -- only want to return our "public" api messages
        --         -- for now let's return them all!
        --         ( aerialModel, maybeOutMsg ) =
        --             AerialUpdate.update aerialMsg model.aerialModel
        --
        --         model2 =
        --             { model | aerialModel = aerialModel }
        --     in
        --         case maybeOutMsg of
        --             Just outMsg ->
        --                 case outMsg of
        --                     AerialTypes.SelfMsg msg ->
        --                         update msg model2
        --
        --                     AerialTypes.MouseClick position ->
        --                         -- this just gives the position, but we can give position + aerialModel
        --                         -- to get the latlng
        --                         -- then we add it to the list of lat lngs.. pretty easy!
        --                         let
        --                             latLng =
        --                                 viewportPointToLatLng model2.aerialModel position
        --
        --                             newMarkers =
        --                                 model.markers ++ [ latLng ]
        --                         in
        --                             { model2 | aerialModel = aerialModel, markers = newMarkers }
        --                                 ! []
        --
        --             Nothing ->
        --                 model2 ! []
        MouseClick ->
            let
                _ =
                    Debug.log "AddPinPlugin MouseClick" True
            in
                model



-- view : Model -> AerialTypes.Model -> Html Msg


view : Model -> Html Msg
view model =
    let
        -- _ =
        --     Debug.log "" True
        markerWrappers =
            model.markers
                -- |> List.map (latLngToViewportPoint aerialModel)
                |>
                    -- Until we figure out how to pass the model along
                    List.map (always { x = 40, y = 40 })
                |> List.map (markerWrapperView markerView)
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
        , onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.succeed MouseClick)
        ]
        []
