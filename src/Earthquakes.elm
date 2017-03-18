module Earthquakes exposing (..)

import GeoJson exposing (FeatureObject, GeoJson, GeoJsonObject(FeatureCollection))
import Html exposing (Html, div, span)
import Http
import Json.Decode as Json
import RemoteData exposing (RemoteData(Loading), RemoteData(Success), WebData)


urlPastWeek4pt5GeoJson : String
urlPastWeek4pt5GeoJson =
    "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_week.geojson"


type alias Model =
    { geojson : WebData GeoJson }


type Msg
    = DataResponse (WebData GeoJson)


empty : Html msg
empty =
    span [] []


getGeojson : Cmd Msg
getGeojson =
    Http.get urlPastWeek4pt5GeoJson GeoJson.decoder
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


view : Model -> Html Msg
view model =
    case model.geojson of
        Success geojson ->
            viewData geojson

        _ ->
            empty


viewData : GeoJson -> Html msg
viewData ( geojson, _ ) =
    case geojson of
        FeatureCollection features ->
            div []
                (List.map viewFeature features)

        _ ->
            empty


viewFeature : FeatureObject -> Html msg
viewFeature feature =
    Json.decodeValue earthquakePropertiesDecoder feature.properties
        |> Result.map
            (\{ magnitude } ->
                div [] []
            )
        |> Result.withDefault empty


earthquakePropertiesDecoder : Json.Decoder { magnitude : Float }
earthquakePropertiesDecoder =
    Json.field "mag" Json.float |> Json.map (\magnitude -> { magnitude = magnitude })



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
