module VectorMath exposing (..)

import Mouse


type alias Vector2DInt =
    { x : Int
    , y : Int
    }


type alias Point2DInt =
    { x : Int
    , y : Int
    }


fromMousePosition : Mouse.Position -> Point2DInt
fromMousePosition mousePosition =
    { x = mousePosition.x
    , y = mousePosition.y
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


addVector : Point2DInt -> Vector2DInt -> Point2DInt
addVector point vector =
    { x = point.x + vector.x
    , y = point.y + vector.y
    }


minusVector : Point2DInt -> Vector2DInt -> Point2DInt
minusVector point vector =
    { x = point.x - vector.x
    , y = point.y - vector.y
    }


scalarMultiply : Vector2DInt -> Int -> Vector2DInt
scalarMultiply v amount =
    { x = v.x * amount
    , y = v.y * amount
    }
