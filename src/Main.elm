module Main exposing (main)

import Html
import View
import Model
import Update
import Types


main : Program Never Types.Model Types.Msg
main =
    Html.program
        { view = View.view
        , update = Update.update
        , subscriptions = \_ -> Sub.none
        , init = ( Model.model, Cmd.none )
        }
