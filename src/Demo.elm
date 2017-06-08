module Demo exposing (Model, Msg, init, update, view, subscriptions)

-- import Geo exposing (LatLng)

import Aerial.Model exposing (viewportPointToLatLng)
import Aerial.Types
import Aerial.Update
import Aerial.View
import Earthquakes
import Html exposing (..)
import Aerial.Model exposing (viewportPointToLatLng)
import Aerial.Types
import Aerial.Update
import Aerial.View


type alias Model =
    { prop1 : String
    , prop2 : Int
    , aerialModel :
        Aerial.Types.Model
    , earthquakes : Earthquakes.Model
    }


setAerialModel : Aerial.Types.Model -> Model -> Model
setAerialModel aerialModel model =
    { model | aerialModel = aerialModel }


init : ( Model, Cmd Msg )
init =
    let
        ( earthquakes, earthquakesMsg ) =
            Earthquakes.init
    in
        { prop1 = "hello"
        , prop2 = 2
        , aerialModel =
            Aerial.Model.init
        , earthquakes = earthquakes
        }
            ! [ Cmd.map EarthquakesMsg earthquakesMsg ]



-- UPDATE


type Msg
    = AerialMsg (Aerial.Types.Msg Msg)
    | EarthquakesMsg Earthquakes.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        AerialMsg aerialMsg ->
            Aerial.Update.update aerialMsg model.aerialModel
                |> \( aerialModel, aerialReturn ) ->
                    model
                        |> setAerialModel aerialModel
                        |> handleAerialReturn aerialReturn

        EarthquakesMsg earthquakesMsg ->
            { model | earthquakes = Earthquakes.update earthquakesMsg model.earthquakes }


handleAerialReturn : Aerial.Types.Return Msg -> Model -> Model
handleAerialReturn return model =
    case return of
        Aerial.Types.OutMsg outMsg ->
            model
                |> updateWithAerialOutMsg outMsg

        Aerial.Types.SelfMsg msg ->
            update msg model

        Aerial.Types.ReturnNothing ->
            model


updateWithAerialOutMsg : Aerial.Types.OutMsg -> Model -> Model
updateWithAerialOutMsg outMsg model =
    case outMsg of
        Aerial.Types.MouseClick position ->
            model


view : Model -> Html Msg
view model =
    let
        pluginLayerView =
            Html.map EarthquakesMsg <|
                Earthquakes.view model.aerialModel model.earthquakes

        aerialViewConfig : Aerial.View.Config Msg
        aerialViewConfig =
            { pluginLayerView = pluginLayerView }

        aerialView =
            (Aerial.View.view aerialViewConfig model.aerialModel)
    in
        div []
            [ Html.map AerialMsg <| aerialView
            , text "hello"
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Earthquakes.subscriptions model.earthquakes |> Sub.map EarthquakesMsg
