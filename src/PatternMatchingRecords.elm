module Main exposing (..)


type alias Person =
    { name : String
    , age : Int
    , location : Location
    }


type alias Location =
    { city : String
    , country : String
    }



-- getCity : Person -> String
-- getCity { name, age, { city, country } as location } =
--     city


getCity : Person -> String
getCity person =
    person.location.city


getCity2 : Person -> String
getCity2 ({ name, age, location } as person) =
    location.city



getCity3 : Person -> String
getCity3 ({ name, age, ({ city, country } as location) } as person) =
    location.city
