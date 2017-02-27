module Constants exposing (..)


slippyTileSize : Int
slippyTileSize =
    256


{-| in meters on the equator
-}
sphericalMercatorEarthRadiusMeters : Float
sphericalMercatorEarthRadiusMeters =
    6378137



{- This isn't really of much use for online maps - only if you wanted to display a
   rectangular fully zoomed out map. Actually in Open Street Map and Google maps
   when fully zoomed out you see a perfect square showing the earth with a
   rediculously distorted north and south pole. My understanding that the
   Spherical Mercator max latitude is an arbitrarily chosen point where suddenly
   the distortion is considered to have a passed a threshold of rediculousness.
-}
-- sphericalMercatorMaxLatitude : Float
-- sphericalMercatorMaxLatitude =
--     85.0511287798
