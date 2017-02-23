module Geo exposing (..)


type alias LatLng =
    { lat : Float
    , lng : Float
    }


type alias Point2D =
    { x : Float
    , y : Float
    }


type alias WorldPoint2D =
    Point2D


type alias OSMTileCoordinate =
    { x : Int
    , y : Int
    }



-- Constants


tileSize : Int
tileSize =
    256


sphericalMercatorEarthRadiusMeters : Float
sphericalMercatorEarthRadiusMeters =
    6378137


sphericalMercatorMaxLatitude : Float
sphericalMercatorMaxLatitude =
    85.0511287798



-- Functions


latLngToTileCoordinates : Int -> LatLng -> OSMTileCoordinate
latLngToTileCoordinates zoom latlng =
    latLngToWorldPoint zoom latlng
        |> toOSMTileCoordinates


toOSMTileCoordinates : WorldPoint2D -> OSMTileCoordinate
toOSMTileCoordinates worldPoint =
    { x = toOSMTileCoordinate worldPoint.x
    , y = toOSMTileCoordinate worldPoint.y
    }


toOSMTileCoordinate : Float -> Int
toOSMTileCoordinate worldCoordinate =
    worldCoordinate / (toFloat tileSize) |> floor


latLngToWorldPoint : Int -> LatLng -> WorldPoint2D
latLngToWorldPoint zoom latlng =
    let
        projectedPoint =
            projectToPoint2D latlng

        -- zoom is an exponential scale, so we need to convert it to a linear scale
        linearScale =
            (tileSize * (2 ^ zoom))
                |> toFloat
    in
        transform epsg3857Transform linearScale projectedPoint


projectToPoint2D : LatLng -> Point2D
projectToPoint2D latlng =
    let
        d =
            pi / 180

        maxLat =
            sphericalMercatorMaxLatitude

        lat =
            max (min maxLat latlng.lat) -maxLat

        sin_ =
            sin (lat * d)

        r =
            sphericalMercatorEarthRadiusMeters
    in
        { x = r * latlng.lng * d
        , y = r * logBase e ((1 + sin_ / 1 - sin_) / 2)
        }


type alias TransformationMatrix =
    { a : Float, b : Float, c : Float, d : Float }


epsg3857Transform : TransformationMatrix
epsg3857Transform =
    let
        scale =
            0.5 / pi * sphericalMercatorEarthRadiusMeters
    in
        TransformationMatrix scale 0.5 -scale 0.5


transform : TransformationMatrix -> Float -> Point2D -> Point2D
transform matrix scale point =
    { x = scale * (matrix.a * point.x + matrix.b)
    , y = scale * (matrix.c * point.y + matrix.d)
    }
