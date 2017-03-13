module Demo exposing (Model, Msg, init, update, view)

-- import Geo exposing (LatLng)

import Html exposing (..)
import Html.Attributes exposing (..)


-- import Html.Events exposing (..)

import Model as AerialModel exposing (viewportPointToLatLng)
import Types as AerialTypes
import Update as AerialUpdate
import View as AerialView
import AddPinPlugin


-- MODEL


type alias Model =
    { prop1 : String
    , prop2 : Int
    , aerialModel :
        AerialTypes.Model
        -- , markers : List LatLng
    , addPinPlugin : AddPinPlugin.Model
    }


init : ( Model, Cmd Msg )
init =
    { prop1 = "hello"
    , prop2 = 2
    , aerialModel =
        AerialModel.init
        -- , markers = [ LatLng -37.814 144.96332, LatLng -33.86785 151.20732 ]
    , addPinPlugin = AddPinPlugin.init
    }
        ! []



-- UPDATE


type Msg
    = AerialMsg (AerialTypes.Msg Msg)
    | AddPinPluginMsg AddPinPlugin.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AerialMsg aerialMsg ->
            let
                -- Here we might get an AerialMsg or one of our own messages.
                --We *could* just return all messages, but we probably
                -- only want to return our "public" api messages
                -- for now let's return them all!
                ( aerialModel, maybeOutMsg ) =
                    AerialUpdate.update aerialMsg model.aerialModel

                model2 =
                    { model | aerialModel = aerialModel }
            in
                case maybeOutMsg of
                    Just outMsg ->
                        case outMsg of
                            AerialTypes.SelfMsg msg ->
                                update msg model2

                            AerialTypes.MouseClick position ->
                                -- this just gives the position, but we can give position + aerialModel
                                -- to get the latlng
                                -- then we add it to the list of lat lngs.. pretty easy!
                                model ! []

                    -- let
                    --     latLng =
                    --         viewportPointToLatLng model2.aerialModel position
                    --
                    --     newMarkers =
                    --         model.markers ++ [ latLng ]
                    -- in
                    --     { model2 | aerialModel = aerialModel, markers = newMarkers }
                    --         ! []
                    Nothing ->
                        model2 ! []

        AddPinPluginMsg addPinPluginMsg ->
            { model | addPinPlugin = AddPinPlugin.update addPinPluginMsg model.addPinPlugin } ! []



-- model ! []
-- in
--     case aerialMsg of
--         AerialTypes.ParentMsg customMsg ->
--             -- Just customMsg ->
--             let
--                 ( newModel, _ ) =
--                     update customMsg model
--             in
--                 { newModel | aerialModel = aerialModel }
--                     ! []
--
--
--         _ ->
--             { model | aerialModel = aerialModel }
--                 ! []
-- VIEW


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
        ]
        []


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
        aerialViewConfig : AerialView.Config Msg
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
            (AerialView.view aerialViewConfig model.aerialModel)
    in
        div []
            [ Html.map AerialMsg <| aerialView
            ]
