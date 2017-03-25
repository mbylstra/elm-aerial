module Demo exposing (Model, Msg, init, update, view, subscriptions)

-- import Geo exposing (LatLng)

import AddPinPlugin
import Aerial.Model exposing (viewportPointToLatLng)
import Aerial.Types
import Aerial.Update
import Aerial.View
import Earthquakes
import Html exposing (..)


-- import Html.Attributes exposing (..)

import Aerial.Model exposing (viewportPointToLatLng)
import Aerial.Types
import Aerial.Update
import Aerial.View


-- import Mouse
-- import Html.Events exposing (..)

import AddPinPlugin


type alias Model =
    { prop1 : String
    , prop2 : Int
    , aerialModel :
        Aerial.Types.Model
        -- , markers : List LatLng
    , addPinPlugin :
        AddPinPlugin.Model
    , earthquakes : Earthquakes.Model
    }


setAerialModel : Aerial.Types.Model -> Model -> Model
setAerialModel aerialModel model =
    { model | aerialModel = aerialModel }


setAddPinPlugin : AddPinPlugin.Model -> Model -> Model
setAddPinPlugin addPinPlugin model =
    { model | addPinPlugin = addPinPlugin }


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
        , addPinPlugin = AddPinPlugin.init
        , earthquakes = earthquakes
        }
            ! [ Cmd.map EarthquakesMsg earthquakesMsg ]



-- UPDATE


type Msg
    = AerialMsg (Aerial.Types.Msg Msg)
    | AddPinPluginMsg AddPinPlugin.Msg
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

        AddPinPluginMsg addPinPluginMsg ->
            { model | addPinPlugin = AddPinPlugin.update addPinPluginMsg model.addPinPlugin model.aerialModel }

        EarthquakesMsg earthquakesMsg ->
            { model | earthquakes = Earthquakes.update earthquakesMsg model.earthquakes }


handleAerialReturn : Aerial.Types.Return Msg -> Model -> Model
handleAerialReturn return model =
    case return of
        Aerial.Types.OutMsg outMsg ->
            model
                |> updateWithAerialOutMsg outMsg
                |> setAddPinPlugin (AddPinPlugin.updateWithAerialOutMsg outMsg model.aerialModel model.addPinPlugin)

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
            -- Html.map AddPinPluginMsg <|
            Html.map EarthquakesMsg <|
                --AddPinPlugin.view model.addPinPlugin aerialModel
                Earthquakes.view model.aerialModel model.earthquakes

        -- So, we should be able to pass this to Config
        aerialViewConfig : Aerial.View.Config Msg
        aerialViewConfig =
            { pluginLayerView = pluginLayerView }

        -- { -- markerView = AddPinPlugin.markerView
        --   -- , markers = model.markers
        --   pluginLayerView =
        --     mappedView
        --     -- unMappedView
        --     -- Html.map AddPinPluginMsg <|
        -- }
        -- Html.map
        -- mappedView
        -- AerialMsg
        -- Hmm, how do we go back the other way???
        aerialView =
            (Aerial.View.view aerialViewConfig model.aerialModel)
    in
        div []
            [ Html.map AerialMsg <| aerialView
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Earthquakes.subscriptions model.earthquakes |> Sub.map EarthquakesMsg
