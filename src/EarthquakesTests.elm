module EarthquakesTests exposing (..)

-- import Aerial.Geo exposing (LatLng)

import Earthquakes exposing (appMillisToWorldMillis, getScreenLifeDurationWorldMillis)
import Expect
import Test exposing (..)


all : Test
all =
    describe "Earthquakes.elm"
        [ describe "getScreenLifeDurationWorldMillis"
            [ test "magniude: 3.0" <|
                \() ->
                    getScreenLifeDurationWorldMillis 3.0
                        |> Expect.equal 1234
            ]
        , describe "screenMillisToWorldMillis"
            [ test "magniude: 3.0" <|
                \() ->
                    appMillisToWorldMillis 1.0
                        |> Expect.equal 1000.0
            ]
        ]
