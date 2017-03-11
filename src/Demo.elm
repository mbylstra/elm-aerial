module Demo exposing (Model, Msg, init, update, view)

import Geo exposing (LatLng)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model as AerialModel
import Types as AerialTypes
import Update as AerialUpdate
import View as AerialView


-- MODEL


type alias Model =
    { prop1 : String
    , prop2 : Int
    , aerialModel : AerialTypes.Model
    }


init : ( Model, Cmd Msg )
init =
    { prop1 = "hello"
    , prop2 = 2
    , aerialModel = AerialModel.init
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
                ( aerialModel, maybeMsg ) =
                    AerialUpdate.update aerialMsg model.aerialModel
            in
                case maybeMsg of
                    Just customMsg ->
                        let
                            ( newModel, _ ) =
                                update customMsg model
                        in
                            { newModel | aerialModel = aerialModel } ! []

                    Nothing ->
                        { model | aerialModel = aerialModel } ! []



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
            , markers = [ LatLng -37.814 144.96332, LatLng -33.86785 151.20732 ]
            }

        aerialView =
            (AerialView.view aerialViewConfig model.aerialModel)
    in
        div []
            [ Html.map AerialMsg <| aerialView
            ]
