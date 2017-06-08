module Main exposing (main)

import Html
import Demo exposing (Model, Msg, view, update, init, subscriptions)


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , init = init
        }
