module Tiles exposing (..)

{-| This is the width in the tiles at 1:1 resolution.
-}

import SlippyTiles


getWorldWidthInTiles : Int -> Int
getWorldWidthInTiles zoom =
    2 ^ zoom


getTileViewZoom : Int -> Float -> Int
getTileViewZoom zoom resolution =
    zoom + (logBase 2 resolution |> floor)


getTileSize : Float -> Int
getTileSize resolution =
    (toFloat SlippyTiles.tileSize) / resolution |> floor
