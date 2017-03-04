module SphericalMercator.Tests exposing (..)

import Test exposing (..)
import Expect
import SphericalMercator
    exposing
        ( latToProjectedRadians
        , latFromProjectedRadians
        , projectLatToMeters
        , projectLngToMeters
        , unprojectLngFromMeters
        , unprojectLatFromMeters
        )
import Geo exposing (LatLng)


million : Float
million =
    1000000.0


alertCanadaLatLng : LatLng
alertCanadaLatLng =
    { lat = 82.5018, lng = 62.3481 }


all : Test
all =
    describe "SphericalMercator.elm"
        [ describe "latToProjectedRadians"
            [ test "from 0.0" <|
                \() ->
                    latToProjectedRadians 0.0
                        |> Expect.all [ Expect.lessThan 0.0001, Expect.greaterThan -0.0001 ]
            , test "from 45.0" <|
                \() ->
                    latToProjectedRadians 45.0
                        |> Expect.all [ Expect.greaterThan (pi / 4), Expect.lessThan (pi / 2) ]
            , test "from -45.0" <|
                \() ->
                    latToProjectedRadians -45.0
                        |> Expect.all [ Expect.lessThan -(pi / 4), Expect.greaterThan -(pi / 2) ]
            , test "from 90.0" <|
                \() ->
                    latToProjectedRadians 90.0
                        |> Expect.greaterThan (pi / 2)
            , test "from -90.0" <|
                \() ->
                    latToProjectedRadians -90.0
                        |> Expect.lessThan (pi / 2)
            ]
        , describe "latFromProjectedRadians"
            [ test "from 0.0" <|
                \() ->
                    latFromProjectedRadians 0.0
                        |> Expect.equal 0.0
            , test "from pi/4" <|
                \() ->
                    latFromProjectedRadians (pi / 4)
                        |> Expect.all [ Expect.lessThan 45.0, Expect.greaterThan 40.0 ]
            , test "from -(pi/4)" <|
                \() ->
                    latFromProjectedRadians -(pi / 4)
                        |> Expect.all [ Expect.lessThan -40.0, Expect.greaterThan -45.0 ]
            , test "from -(pi/4)" <|
                \() ->
                    let
                        result =
                            latFromProjectedRadians (pi / 4)
                    in
                        (latToProjectedRadians result)
                            |> Expect.all [ Expect.lessThan ((pi / 4) + 0.001), Expect.greaterThan ((pi / 4) - 0.001) ]
            ]
        , describe "projectLngToMeters"
            [ test "from 90" <|
                \() ->
                    projectLngToMeters 90.0
                        |> Expect.all [ Expect.lessThan (11 * million), Expect.greaterThan (9 * million) ]
            , test "from 180.0" <|
                \() ->
                    projectLngToMeters 180.0
                        |> Expect.all [ Expect.lessThan (21 * million), Expect.greaterThan (19 * million) ]
            , test "from -90.0" <|
                \() ->
                    projectLngToMeters -90.0
                        |> Expect.all [ Expect.greaterThan (-11 * million), Expect.lessThan (-9 * million) ]
            ]
        , describe "projectLatToMeters"
            [ test "from 45.0 (eg Quebec)" <|
                \() ->
                    projectLatToMeters 45.0
                        |> Expect.all [ Expect.lessThan (6 * million), Expect.greaterThan (5 * million) ]
            , test "from -45.0 (eg New Zealand south island)" <|
                \() ->
                    projectLatToMeters -45.0
                        |> Expect.all [ Expect.greaterThan (-6 * million), Expect.lessThan (-5 * million) ]
            , test "from 45.0 (eg Quebec)" <|
                \() ->
                    projectLatToMeters 45.0
                        |> Expect.all [ Expect.lessThan (6 * million), Expect.greaterThan (5 * million) ]
            , test "from Alert Canada latitude" <|
                \() ->
                    projectLatToMeters alertCanadaLatLng.lat
                        |> Expect.all [ Expect.greaterThan (17 * million), Expect.lessThan (20 * million) ]
            , test "from Alert Canada latitude * -1" <|
                \() ->
                    projectLatToMeters (alertCanadaLatLng.lat * -1)
                        |> Expect.all [ Expect.lessThan (-17 * million), Expect.greaterThan (-20 * million) ]
            , test "from Alert Canada latitude and back" <|
                \() ->
                    let
                        resultMeters =
                            projectLatToMeters alertCanadaLatLng.lat
                    in
                        unprojectLatFromMeters resultMeters
                            |> Expect.all [ Expect.lessThan (alertCanadaLatLng.lat + 0.001), Expect.greaterThan (alertCanadaLatLng.lat - 0.001) ]
            ]
        ]
