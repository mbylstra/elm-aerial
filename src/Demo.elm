module Demo exposing (Model, Msg, init, update, view)

-- import Geo exposing (LatLng)

import AddPinPlugin
import Html exposing (..)


-- import Html.Attributes exposing (..)

import Aerial.Model exposing (viewportPointToLatLng)
import Aerial.Types
import Aerial.Update
import Aerial.View


-- import Mouse
-- import Html.Events exposing (..)

import AddPinPlugin


-- MODEL


type alias Model =
    { prop1 : String
    , prop2 : Int
    , aerialModel :
        Aerial.Types.Model
        -- , markers : List LatLng
    , addPinPlugin : AddPinPlugin.Model
    }


setAerialModel : Aerial.Types.Model -> Model -> Model
setAerialModel aerialModel model =
    { model | aerialModel = aerialModel }


setAddPinPlugin : AddPinPlugin.Model -> Model -> Model
setAddPinPlugin addPinPlugin model =
    { model | addPinPlugin = addPinPlugin }


init : Model
init =
    { prop1 = "hello"
    , prop2 = 2
    , aerialModel =
        Aerial.Model.init
    , addPinPlugin = AddPinPlugin.init
    }



-- UPDATE


type Msg
    = AerialMsg (Aerial.Types.Msg Msg)
    | AddPinPluginMsg AddPinPlugin.Msg


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
        aerialModel =
            model.aerialModel

        -- myButton : Html (AerialTypes.Msg Msg)
        -- myButton =
        --     button [ onClick (AerialTypes.CustomMsg ButtonClicked) ] [ text "Button" ]
        -- unMappedView : AerialTypes.Model -> Html AddPinPlugin.Msg
        unMappedView : Html AddPinPlugin.Msg
        unMappedView =
            AddPinPlugin.view model.addPinPlugin aerialModel

        -- mappedView : AerialTypes.Model -> Html Msg
        mappedView : Html Msg
        mappedView =
            -- Html.map AddPinPluginMsg (unMappedView model)
            Html.map AddPinPluginMsg unMappedView

        -- So, we should be able to pass this to Config
        aerialViewConfig : Aerial.View.Config Msg
        aerialViewConfig =
            { pluginLayerView = mappedView }

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
