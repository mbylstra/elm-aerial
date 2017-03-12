module Update exposing (..)

import Model exposing (cleanLat, cleanLatLng, cleanLng, getViewportCenter, isInViewport, setZoom, viewportPointToLatLng, zoomAtCursor)
import Types exposing (Msg(..), OutMsg(..), Model)
import VectorMath exposing (difference)


update : Msg parentMsg -> Model -> ( Model, Maybe (OutMsg parentMsg) )
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
                        |> cleanLng

                newLatLng =
                    { latLng | lng = newLng }
            in
                ( { model | latLng = newLatLng }, Nothing )

        UpdateLat latString ->
            let
                latLng =
                    model.latLng

                newLat =
                    latString
                        |> String.toFloat
                        |> Result.withDefault (latLng.lat)
                        |> cleanLat

                newLatLng =
                    { latLng | lat = newLat }
            in
                ( { model | latLng = newLatLng }, Nothing )

        UpdateZoom zoomString ->
            let
                newModel =
                    zoomString
                        |> String.toInt
                        |> Result.map (setZoom model)
                        |> Result.withDefault model
            in
                ( newModel, Nothing )

        MouseDown position ->
            let
                maybeMouseOver =
                    model.maybeMouseOver
                        |> Maybe.map
                            (\mouseOverState -> { mouseOverState | down = Just { startPosition = position } })
            in
                ( { model | maybeMouseOver = maybeMouseOver }, Nothing )

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
                                            difference mouseDownState.startPosition mouseOverState.position

                                        viewportCenter =
                                            getViewportCenter model

                                        newCenterLatLng =
                                            VectorMath.minusVector viewportCenter mouseMovedVector
                                                |> viewportPointToLatLng model
                                                |> cleanLatLng

                                        -- currentViewpoint
                                        -- so just thinking about lng/x, we need to move the center lat *backwards* (minus) from mouseMovedVector.x
                                        -- so we get x at (mapCenter.x - mouseMovedVector.x)
                                        -- and get the latlng for that x
                                        -- Let's implement getting latlng when clicking on the map
                                        -- mapCenter in pixels should be model function
                                    in
                                        { model
                                            | maybeMouseOver = Just <| { mouseOverState | down = Nothing }
                                            , latLng = newCenterLatLng
                                        }

                                -- This is where we need to recalculate the center lat lng
                                Nothing ->
                                    -- This would only happen if the user put the mouse down, then mouseovered the map. There isn't really anything
                                    -- useful we should do in this case, so do nothing.
                                    model
            in
                ( newModel, Nothing )

        MouseMove position ->
            -- the annoying thing is that we need to preserver the mouseDown state
            -- which is packaged inside the maybe
            let
                maybeMouseOver =
                    model.maybeMouseOver
                        |> Maybe.map
                            (\mouseOverState -> { mouseOverState | position = position })
            in
                ( { model | maybeMouseOver = maybeMouseOver }, Nothing )

        MouseEnter position ->
            ( { model | maybeMouseOver = Just { position = position, down = Nothing } }, Nothing )

        MouseLeave position ->
            if (Debug.log "isInViewport" <| isInViewport model position) then
                ( model, Nothing )
            else
                ( { model | maybeMouseOver = Nothing }, Nothing )

        MouseClickEvent position ->
            -- let
            --     _ =
            --         Debug.log "latlng" (viewportPointToLatLng model position)
            -- _ =
            --     Debug.log "position" position
            -- in
            ( model, Just <| MouseClick position )

        MouseWheel wheelEvent ->
            let
                newModel =
                    case model.maybeMouseOver of
                        Just mouseOverState ->
                            if wheelEvent.deltaY < 0.0 then
                                zoomAtCursor model True
                            else if wheelEvent.deltaY > 0.0 then
                                zoomAtCursor model False
                            else
                                model

                        Nothing ->
                            model
            in
                ( newModel, Nothing )

        ParentMsg parentMsg ->
            -- now whattawe do??
            -- todo convert to OutMsg
            ( model, Just (SelfMsg parentMsg) )
