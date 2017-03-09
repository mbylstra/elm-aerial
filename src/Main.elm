module Main exposing (main)

import Html
import Demo exposing (Model, Msg, view, update, init)


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = init
        }
