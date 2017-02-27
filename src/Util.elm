module Util exposing (..)


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs
