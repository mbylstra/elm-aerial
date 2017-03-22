module Earthquakes exposing (..)

import Aerial.Geo exposing (LatLng)
import Aerial.Markers exposing (markerHolder, simpleMarker)
import Aerial.Types
import GeoJson exposing (FeatureObject, GeoJson, GeoJsonObject(FeatureCollection))
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Http
import Json.Decode as Json
import RemoteData exposing (RemoteData(Loading), RemoteData(Success), WebData)


urlPastWeek4pt5GeoJson : String
urlPastWeek4pt5GeoJson =
    "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_week.geojson"


urlPastDay2pt5GeoJson : String
urlPastDay2pt5GeoJson =
    "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_day.geojson"


urlPastWeek2pt5GeoJson : String
urlPastWeek2pt5GeoJson =
    "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_week.geojson"


urlPastMonth2pt5GeoJson : String
urlPastMonth2pt5GeoJson =
    "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_month.geojson"


type alias Model =
    { geojson : WebData GeoJson }


type Msg
    = DataResponse (WebData GeoJson)


empty : Html msg
empty =
    span [] []


getGeojson : Cmd Msg
getGeojson =
    --Http.get urlPastWeek4pt5GeoJson GeoJson.decoder
    -- Http.get urlPastDay2pt5GeoJson GeoJson.decoder
    -- Http.get urlPastWeek2pt5GeoJson GeoJson.decoder
    Http.get urlPastMonth2pt5GeoJson GeoJson.decoder
        |> RemoteData.sendRequest
        |> Cmd.map DataResponse


init : ( Model, Cmd Msg )
init =
    ( { geojson = Loading }
    , getGeojson
    )


update : Msg -> Model -> Model
update msg model =
    case msg of
        DataResponse response ->
            { model | geojson = response }


view : Aerial.Types.Model -> Model -> Html Msg
view aerialModel model =
    case model.geojson of
        Success geojson ->
            viewData aerialModel geojson

        _ ->
            empty


viewData : Aerial.Types.Model -> GeoJson -> Html msg
viewData aerialModel ( geojson, _ ) =
    case geojson of
        FeatureCollection features ->
            div []
                (List.map (viewFeature aerialModel) features)

        _ ->
            empty


viewFeature : Aerial.Types.Model -> FeatureObject -> Html msg
viewFeature aerialModel feature =
    case feature.geometry of
        Just (GeoJson.Point position) ->
            Json.decodeValue earthquakePropertiesDecoder feature.properties
                |> Result.map
                    (\{ magnitude } ->
                        position
                            |> positionToLatLng
                            |> markerHolder (earthquakeMarker magnitude) aerialModel
                    )
                |> Result.withDefault empty

        _ ->
            empty


earthquakePropertiesDecoder : Json.Decoder { magnitude : Float }
earthquakePropertiesDecoder =
    Json.field "mag" Json.float |> Json.map (\magnitude -> { magnitude = magnitude })


positionToLatLng : GeoJson.Position -> LatLng
positionToLatLng ( lng, lat, _ ) =
    { lat = lat, lng = lng }



-- "properties": {
-- "mag": 4.8,
-- "place": "23km N of Kabanjahe, Indonesia",
-- "time": 1489834270590,
-- "updated": 1489835434727,
-- "tz": 420,
-- "url": "https://earthquake.usgs.gov/earthquakes/eventpage/us20008szl",
-- "detail": "https://earthquake.usgs.gov/earthquakes/feed/v1.0/detail/us20008szl.geojson",
-- "felt": 1,
-- "cdi": 2,
-- "mmi": null,
-- "alert": null,
-- "status": "reviewed",
-- "tsunami": 0,
-- "sig": 355,
-- "net": "us",
-- "code": "20008szl",
-- "ids": ",us20008szl,",
-- "sources": ",us,",
-- "types": ",dyfi,geoserve,origin,phase-data,",
-- "nst": null,
-- "dmin": 0.767,
-- "rms": 1.01,
-- "gap": 61,
-- "magType": "mb",
-- "type": "earthquake",
-- "title": "M 4.8 - 23km N of Kabanjahe, Indonesia"
-- },


earthquakeMarker : Float -> Html msg
earthquakeMarker magnitude =
    let
        radius =
            magnitudeToPixelRadius magnitude

        width =
            radius * 2

        top =
            -radius

        left =
            -radius
    in
        div
            [ style
                [ ( "width", toString width ++ "px" )
                , ( "height", toString width ++ "px" )
                , ( "position", "absolute" )
                , ( "top", toString top ++ "px" )
                , ( "left", toString left ++ "px" )
                , ( "border", "1px solid black" )
                , ( "border-radius", toString radius ++ "px" )
                , ( "background-color", "rgba(0,0,0,0.1)" )
                , ( "z-index", "10" )
                ]
            ]
            [ text <| toString magnitude ]


magnitudeToPixelRadius : Float -> Int
magnitudeToPixelRadius magnitude =
    let
        linearValue =
            10 ^ (magnitude - 1)
    in
        sqrt (linearValue / pi) |> round
