module Main exposing (..)


type CatType
    = Siamese
    | Persian
    | Ragdoll
    | Bengal


type Pet
    = Cat CatType
    | Dog
    | Hamster


myPet : Pet
myPet =
    Cat Bengal


isBengalCat : Pet -> Bool
isBengalCat myPet =
    case myPet of
        Cat Bengal ->
            True

        _ ->
            False
