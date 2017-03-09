module Util exposing (..)


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


ln : Float -> Float
ln x =
    logBase e x


circumference : Float -> Float
circumference r =
    2 * pi * r


{-| Convert degrees to radians
   The stdlib degrees function is confusing, so I'm renaming it
-}
toRadians : Float -> Float
toRadians =
    degrees


toDegrees : Float -> Float
toDegrees radians =
    radians * 180.0 / pi


fmod : Float -> Float -> Float
fmod a b =
    a - b * (toFloat <| floor (a / b))
