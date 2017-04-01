module Earthquakes exposing (..)

import Aerial.Geo exposing (LatLng)
import Aerial.SvgMarkers exposing (svgMarkerHolder)
import Aerial.Types
import AnimationFrame
import GeoJson exposing (FeatureObject, GeoJson, GeoJsonObject(FeatureCollection))
import Html exposing (Html, div, span)
import Html.Attributes
import Http
import Json.Decode as Json
import RemoteData exposing (RemoteData(Loading), RemoteData(Success), WebData)
import Svg exposing (Svg, g, svg)
import Svg.Attributes exposing (fillOpacity, strokeOpacity, stroke, fill, style)
import Svg.Extra exposing (circle)
import Time exposing (Time, every, millisecond, second)


-- import Html.Attributes exposing (style)

import Http
import Json.Decode as Json
import RemoteData exposing (RemoteData(Loading), RemoteData(Success), WebData)
import Time exposing (Time, every, millisecond, second)
import Svg.Extra exposing (circle)
import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (style)


-- MODEL


howManyMinutesForADay : Float
howManyMinutesForADay =
    0.01


minutesInADay : Float
minutesInADay =
    24.0 * 60.0


timeScale : Float
timeScale =
    minutesInADay / howManyMinutesForADay


fps : Float
fps =
    60.0


frameDurationMillis : Float
frameDurationMillis =
    (1.0 / fps) * 1000.0



-- a very rough guess!


appMillisToWorldMillis : Float -> Float
appMillisToWorldMillis appMillis =
    appMillis * timeScale


frameDurationWorldMillis : Float
frameDurationWorldMillis =
    appMillisToWorldMillis frameDurationMillis



-- 360
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
    , screenLifeDurationWorldMillis :
        Int
        -- This is derived data, so I'm not sure this is the best place for it.
        -- There are two conflicting philosophies:
        -- 1. Only store the single source of truth in the model
        -- 2. Flat is better than nested
        -- I think the best answer is to memoise the getScreenLifeDurationWorldMillis
        -- function, but there is no Core support for memoise, and the community
        -- packages are quite clunky.
    }


type alias Model =
    { earthquakesResponse :
        WebData (List Earthquake)
    , currentTime : Int
    }


type Msg
    = DataResponse (WebData (List Earthquake))
    | Tick Time


getScreenLifeDurationWorldMillis : Float -> Float
getScreenLifeDurationWorldMillis magnitude =
    let
        maxRadius =
            400

        maxDurationMillis =
            3000

        radius =
            Debug.log "radius" <|
                magnitudeToPixelRadius magnitude

        clippedRadius =
            Debug.log "clippedRadius" <|
                min radius maxRadius

        durationScreenMillis =
            Debug.log "durationScreenMillis" <|
                ((toFloat clippedRadius) / (toFloat maxRadius))
                    * maxDurationMillis
    in
        appMillisToWorldMillis durationScreenMillis



-- empty : Html msg
-- empty =
--     span [] []


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
                    { model | currentTime = model.currentTime + (floor frameDurationWorldMillis) }

                _ ->
                    model


view : Aerial.Types.Model -> Model -> Svg Msg
view aerialModel model =
    case model.earthquakesResponse of
        Success earthquakes ->
            viewData aerialModel model earthquakes model.currentTime

        _ ->
            empty


isCurrentEarthquake : Int -> Earthquake -> Bool
isCurrentEarthquake currentTime earthquake =
    let
        screenLifeDurationMillis =
            earthquake.screenLifeDurationWorldMillis

        earthquakeStartTime =
            -- Debug.log "earthquakeTime" <|
            earthquake.time

        earthquakeEndTime =
            earthquakeStartTime + screenLifeDurationMillis
    in
        (earthquakeStartTime <= currentTime) && (earthquakeEndTime >= currentTime)


viewData : Aerial.Types.Model -> Model -> List Earthquake -> Int -> Svg msg
viewData aerialModel model earthquakes currentTime =
    let
        -- _ =
        --     Debug.log "currentTime" currentTime
        -- nextTime =
        --     -- Debug.log "nextTime" <|
        --     currentTime
        --         + frameDurationWorldMillis
        --
        currentEarthquakes =
            -- Debug.log "currentEarthquakes" <|
            List.filter (isCurrentEarthquake currentTime) earthquakes

        svgStyle =
            "width:" ++ toString aerialModel.mapWidthPx ++ "px;" ++ "height:" ++ toString aerialModel.mapHeightPx ++ "px;"

        divStyle =
            [ ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", "0" )
            , ( "z-index", "100" )
            ]
    in
        div [ Html.Attributes.style divStyle ]
            [ svg
                [ Svg.Attributes.style svgStyle ]
                (List.map (earthquakeView aerialModel model) currentEarthquakes)
            ]


earthquakeView : Aerial.Types.Model -> Model -> Earthquake -> Svg msg
earthquakeView aerialModel model earthquake =
    svgMarkerHolder
        (earthquakeMarker model earthquake)
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
                        , screenLifeDurationWorldMillis = getScreenLifeDurationWorldMillis magnitude |> floor
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


toPx : Int -> String
toPx i =
    toString i ++ "px"


earthquakeMarker : Model -> Earthquake -> Svg msg
earthquakeMarker model earthquake =
    let
        radius =
            magnitudeToPixelRadius earthquake.magnitude

        screenLifeDurationWorldMillis =
            earthquake.screenLifeDurationWorldMillis

        earthquakeStartTime =
            -- Debug.log "earthquakeTime" <|
            earthquake.time

        earthquakeEndTime =
            earthquakeStartTime + screenLifeDurationWorldMillis

        fractionCompleted =
            1.0 - ((toFloat (earthquakeEndTime - model.currentTime)) / (toFloat screenLifeDurationWorldMillis))

        currRadius =
            (toFloat radius) * fractionCompleted |> floor

        opacity =
            1.0 - fractionCompleted

        -- get completed percentage by
        -- getting earthquake start time,
        -- earthquake end time, current time
        -- width =
        --     currRadius * 2
        --
        -- top =
        --     -currRadius
        --
        -- left =
        --     -currRadius
        --
        -- widthPx =
        --     toPx width
        -- radiusPx =
        --     toPx currRadius
        earthquakeColor =
            "rgb(198, 15, 61)"
    in
        g []
            [ circle
                0
                0
                currRadius
                [ stroke earthquakeColor
                , fill earthquakeColor
                  --, opacity |> toString |> strokeOpacity
                , strokeOpacity "0"
                , 0.5 * opacity |> toString |> fillOpacity
                ]
            , circle
                0
                0
                2
                [ fill earthquakeColor
                , opacity * 4.0 |> toString |> fillOpacity
                ]
            ]



-- [ style
--     [ ( "width", widthPx )
--     , ( "height", widthPx )
--     , ( "position", "absolute" )
--     , ( "top", toString top ++ "px" )
--     , ( "left", toString left ++ "px" )
--     , ( "border", "1px solid black" )
--     , ( "border-radius", toString radius ++ "px" )
--     , ( "background-color", "rgba(0,0,0,0.1)" )
--     , ( "z-index", "10" )
--     ]
-- ]
-- [ text <| toString magnitude ]


magnitudeToPixelRadius : Float -> Int
magnitudeToPixelRadius magnitude =
    let
        linearValue =
            10 ^ (magnitude - 1)
    in
        sqrt (linearValue / pi) |> round


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Time.every (Time.second * 360) Tick
    AnimationFrame.diffs Tick


empty : Svg msg
empty =
    g [] []
