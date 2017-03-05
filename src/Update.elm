module Update exposing (..)

import Types exposing (..)
import VectorMath exposing (difference)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLng lngString ->
            let
                latLng =
                    model.latLng

                newLng =
                    lngString
                        |> String.toFloat
                        |> Result.withDefault (latLng.lng)

                newLatLng =
                    { latLng | lng = newLng }
            in
                { model | latLng = newLatLng } ! []

        UpdateLat latString ->
            let
                latLng =
                    model.latLng

                newLat =
                    latString
                        |> String.toFloat
                        |> Result.withDefault (latLng.lat)

                newLatLng =
                    { latLng | lat = newLat }
            in
                { model | latLng = newLatLng } ! []

        UpdateZoom zoomString ->
            let
                newModel =
                    zoomString
                        |> String.toInt
                        |> Result.map (\zoom -> { model | zoom = zoom })
                        |> Result.withDefault model
            in
                newModel ! []

        MouseDown position ->
            let
                maybeMouseOver =
                    model.maybeMouseOver
                        |> Maybe.map
                            (\mouseOverState -> { mouseOverState | down = Just { startPosition = position } })
            in
                { model | maybeMouseOver = maybeMouseOver } ! []

        MouseUp ->
            let
                newModel =
                    case model.maybeMouseOver of
                        Nothing ->
                            -- we don't care about mouse up events if the mouse isn't over the map (it's also impossible for this to happen)
                            model

                        Just mouseOverState ->
                            case mouseOverState.down of
                                Just mouseDownState ->
                                    let
                                        mouseMovedVector =
                                            Debug.log "mouseMovedVector" (difference mouseDownState.startPosition mouseOverState.position)

                                        -- so just thinking about lng/x, we need to move the center lat *backwards* (minus) from mouseMovedVector.x
                                        -- so we get x at (mapCenter.x - mouseMovedVector.x)
                                        -- and get the latlng for that x
                                        -- Let's implement getting latlng when clicking on the map
                                        -- mapCenter in pixels should be model function
                                    in
                                        { model | maybeMouseOver = Just <| { mouseOverState | down = Nothing } }

                                -- This is where we need to recalculate the center lat lng
                                Nothing ->
                                    -- This would only happen if the user put the mouse down, then mouseovered the map. There isn't really anything
                                    -- useful we should do in this case, so do nothing.
                                    model
            in
                newModel ! []

        MouseMove position ->
            -- the annoying thing is that we need to preserver the mouseDown state
            -- which is packaged inside the maybe
            let
                maybeMouseOver =
                    model.maybeMouseOver
                        |> Maybe.map
                            (\mouseOverState -> { mouseOverState | position = position })
            in
                { model | maybeMouseOver = maybeMouseOver } ! []

        MouseEnter position ->
            { model | maybeMouseOver = Just { position = position, down = Nothing } } ! []

        MouseLeave ->
            { model | maybeMouseOver = Nothing } ! []
