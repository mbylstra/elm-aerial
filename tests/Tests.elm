module Tests exposing (..)

import Test exposing (..)
import Aerial.SphericalMercator.Tests
import Aerial.SlippyTiles.Tests
import EarthquakesTests


all : Test
all =
    describe "Main Test Suite"
        [ Aerial.SphericalMercator.Tests.all
        , Aerial.SlippyTiles.Tests.all
        , EarthquakesTests.all
        ]
