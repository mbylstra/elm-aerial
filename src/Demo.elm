module Demo exposing (Model, Msg, init, update, view)

import Html exposing (..)


-- import Html.Attributes exposing (..)

import Html.Events exposing (..)
import Types as AerialTypes
import View as AerialView
import Update as AerialUpdate
import Model as AerialModel


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
    = M1
    | M2
    | M3
    | AerialMsg AerialTypes.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        M1 ->
            { model | prop2 = 1 } ! []

        M2 ->
            { model | prop2 = 2 } ! []

        M3 ->
            { model | prop2 = 3 } ! []

        AerialMsg aerialMsg ->
            let
                ( aerialModel, aerialCmd ) =
                    AerialUpdate.update aerialMsg model.aerialModel
            in
                { model | aerialModel = aerialModel } ! [ Cmd.map AerialMsg aerialCmd ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick M1 ] [ text "M1" ]
        , div
            []
            [ text (toString model) ]
        , button [ onClick M2 ] [ text "M2" ]
        , Html.map AerialMsg <| AerialView.view model.aerialModel
        ]
