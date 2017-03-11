module Demo exposing (Model, Msg, init, update, view)

import Geo exposing (LatLng)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model as AerialModel exposing (viewportPointToLatLng)
import Types as AerialTypes
import Update as AerialUpdate
import View as AerialView


-- MODEL


type alias Model =
    { prop1 : String
    , prop2 : Int
    , aerialModel : AerialTypes.Model
    , markers : List LatLng
    }


init : ( Model, Cmd Msg )
init =
    { prop1 = "hello"
    , prop2 = 2
    , aerialModel = AerialModel.init
    , markers = [ LatLng -37.814 144.96332, LatLng -33.86785 151.20732 ]
    }
        ! []



-- UPDATE


type Msg
    = AerialMsg (AerialTypes.Msg Msg)
    | ButtonClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        ButtonClicked ->
            let
                _ =
                    Debug.log "button clicked" True
            in
                { model | prop2 = 3 } ! []

        AerialMsg aerialMsg ->
            let
                -- Here we might get an AerialMsg or one of our own messages.
                --We *could* just return all messages, but we probably
                -- only want to return our "public" api messages
                -- for now let's return them all!
                aerialModel =
                    AerialUpdate.update aerialMsg model.aerialModel
            in
                case aerialMsg of
                    AerialTypes.CustomMsg customMsg ->
                        -- Just customMsg ->
                        let
                            ( newModel, _ ) =
                                update customMsg model
                        in
                            { newModel | aerialModel = aerialModel }
                                ! []

                    AerialTypes.MouseDown position ->
                        -- this just gives the position, but we can give position + aerialModel
                        -- to get the latlng
                        -- then we add it to the list of lat lngs.. pretty easy!
                        let
                            latLng =
                                viewportPointToLatLng model.aerialModel position

                            newMarkers =
                                model.markers ++ [ latLng ]
                        in
                            { model | aerialModel = aerialModel, markers = newMarkers }
                                ! []

                    _ ->
                        { model | aerialModel = aerialModel }
                            ! []



-- VIEW


markerView : Html Msg
markerView =
    div
        [ style
            [ ( "width", "10px" )
            , ( "height", "10px" )
            , ( "position", "absolute" )
            , ( "top", "-5px" )
            , ( "left", "-5px" )
            , ( "border", "1px solid black" )
            , ( "border-radius", "5px" )
            , ( "background-color", "rgba(0,0,0,0.5)" )
            , ( "z-index", "10" )
            ]
        , onClick ButtonClicked
        ]
        []


view : Model -> Html Msg
view model =
    let
        -- myButton : Html (AerialTypes.Msg Msg)
        -- myButton =
        --     button [ onClick (AerialTypes.CustomMsg ButtonClicked) ] [ text "Button" ]
        aerialViewConfig : AerialView.Config Msg
        aerialViewConfig =
            { markerView = markerView
            , markers = model.markers
            }

        aerialView =
            (AerialView.view aerialViewConfig model.aerialModel)
    in
        div []
            [ Html.map AerialMsg <| aerialView
            ]
