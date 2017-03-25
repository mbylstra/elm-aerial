module Earthquakes exposing (..)

import Aerial.Geo exposing (LatLng)
import Aerial.Markers exposing (markerHolder, simpleMarker)
import Aerial.Types
import AnimationFrame
import GeoJson exposing (FeatureObject, GeoJson, GeoJsonObject(FeatureCollection))
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Http
import Json.Decode as Json
import RemoteData exposing (RemoteData(Loading), RemoteData(Success), WebData)
import Time exposing (Time, every, millisecond, second)


-- MODEL


frameDurationWorldMinutes : Int
frameDurationWorldMinutes =
    60


frameDurationWorldMillis : Int
frameDurationWorldMillis =
    frameDurationWorldMinutes * 60 * 1000



--one day
-- one hour


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


type alias Earthquake =
    { latLng : LatLng
    , time : Int
    , magnitude : Float
    }


type alias Model =
    { earthquakesResponse :
        WebData (List Earthquake)
    , currentTime : Int
    }


type Msg
    = DataResponse (WebData (List Earthquake))
    | Tick Time


empty : Html msg
empty =
    span [] []


decoder : Json.Decoder (List Earthquake)
decoder =
    GeoJson.decoder |> Json.map extractEarthquakes


getGeojson : Cmd Msg
getGeojson =
    --Http.get urlPastWeek4pt5GeoJson GeoJson.decoder
    -- Http.get urlPastDay2pt5GeoJson GeoJson.decoder
    -- Http.get urlPastWeek2pt5GeoJson GeoJson.decoder
    Http.get urlPastMonth2pt5GeoJson decoder
        |> RemoteData.sendRequest
        |> Cmd.map DataResponse


init : ( Model, Cmd Msg )
init =
    ( { earthquakesResponse = Loading, currentTime = 0 }
    , getGeojson
    )


update : Msg -> Model -> Model
update msg model =
    case msg of
        DataResponse earthquakesResponse ->
            let
                model2 =
                    { model | earthquakesResponse = earthquakesResponse }
            in
                case earthquakesResponse of
                    Success earthquakes ->
                        { model2 | currentTime = List.head earthquakes |> Maybe.map .time |> Maybe.withDefault -1 }

                    _ ->
                        model2

        Tick _ ->
            case model.earthquakesResponse of
                Success _ ->
                    { model | currentTime = model.currentTime + frameDurationWorldMillis }

                _ ->
                    model


view : Aerial.Types.Model -> Model -> Html Msg
view aerialModel model =
    case model.earthquakesResponse of
        Success earthquakes ->
            viewData aerialModel earthquakes model.currentTime

        _ ->
            empty


isCurrentEarthquake : Int -> Int -> Earthquake -> Bool
isCurrentEarthquake currentTime nextTime earthquake =
    let
        earthquakeTime =
            -- Debug.log "earthquakeTime" <|
            earthquake.time
    in
        (earthquake.time >= currentTime) && (earthquake.time < nextTime)


viewData : Aerial.Types.Model -> List Earthquake -> Int -> Html msg
viewData aerialModel earthquakes currentTime =
    let
        -- _ =
        --     Debug.log "currentTime" currentTime
        nextTime =
            -- Debug.log "nextTime" <|
            currentTime
                + frameDurationWorldMillis

        --
        currentEarthquakes =
            -- Debug.log "currentEarthquakes" <|
            List.filter (isCurrentEarthquake currentTime nextTime) earthquakes
    in
        div [] (List.map (earthquakeView aerialModel) currentEarthquakes)


earthquakeView : Aerial.Types.Model -> Earthquake -> Html msg
earthquakeView aerialModel earthquake =
    markerHolder
        (earthquakeMarker earthquake.magnitude)
        aerialModel
        earthquake.latLng



-- earthquakeDecoder


extractEarthquakes : GeoJson -> List Earthquake
extractEarthquakes ( geojson, _ ) =
    case geojson of
        FeatureCollection features ->
            List.map extractEarthquakeFromFeature features
                |> List.filterMap (identity)
                |> List.reverse

        _ ->
            []


extractEarthquakeFromFeature : FeatureObject -> Maybe Earthquake
extractEarthquakeFromFeature feature =
    case feature.geometry of
        Just (GeoJson.Point position) ->
            Json.decodeValue earthquakePropertiesDecoder feature.properties
                |> Result.map
                    (\{ magnitude, time } ->
                        { latLng = positionToLatLng position
                        , time = time
                        , magnitude = magnitude
                        }
                    )
                |> Result.toMaybe

        _ ->
            Nothing


type alias EarthquakeProperties =
    { magnitude : Float
    , time : Int
    }


earthquakePropertiesDecoder : Json.Decoder EarthquakeProperties
earthquakePropertiesDecoder =
    Json.map2 EarthquakeProperties
        (Json.field "mag" Json.float)
        (Json.field "time" Json.int)


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


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick
