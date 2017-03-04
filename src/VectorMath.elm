module VectorMath exposing (..)


type alias Vector2DInt =
    { x : Int
    , y : Int
    }


type alias Point2DInt =
    { x : Int
    , y : Int
    }


{-| This is completely made up. I have no idea if difference is a technical
term for this!
-}
difference : Point2DInt -> Point2DInt -> Vector2DInt
difference pointA pointB =
    { x = pointB.x - pointA.x
    , y = pointB.y - pointA.y
    }


{-| Yeah this is pretty dum and should just do multiplication against -1, button
why not
-}
negate : Point2DInt -> Point2DInt
negate p =
    { x = -p.x, y = -p.y }
