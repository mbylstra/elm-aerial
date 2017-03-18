module Update exposing (..)

import Model exposing (cleanLat, cleanLatLng, cleanLng, getViewportCenter, isInViewport, setZoom, viewportPointToLatLng, zoomAtCursor)
import Types exposing (Model, Msg(..), Return(OutMsg), OutMsg(..), Return, Return(ReturnNothing), Return(SelfMsg))
import VectorMath exposing (difference)


update : Msg parentMsg -> Model -> ( Model, Return parentMsg )
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
                ( { model | latLng = newLatLng }, ReturnNothing )

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
                ( { model | latLng = newLatLng }, ReturnNothing )

        UpdateZoom zoomString ->
            let
                newModel =
                    zoomString
                        |> String.toInt
                        |> Result.map (setZoom model)
                        |> Result.withDefault model
            in
                ( newModel, ReturnNothing )

        MouseDown position ->
            let
                maybeMouseOver =
                    model.maybeMouseOver
                        |> Maybe.map
                            (\mouseOverState -> { mouseOverState | down = Just { startPosition = position } })
            in
                ( { model | maybeMouseOver = maybeMouseOver }, ReturnNothing )

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
                ( newModel, ReturnNothing )

        MouseMove position ->
            -- the annoying thing is that we need to preserver the mouseDown state
            -- which is packaged inside the maybe
            let
                maybeMouseOver =
                    model.maybeMouseOver
                        |> Maybe.map
                            (\mouseOverState -> { mouseOverState | position = position })
            in
                ( { model | maybeMouseOver = maybeMouseOver }, ReturnNothing )

        MouseEnter position ->
            ( { model | maybeMouseOver = Just { position = position, down = Nothing } }, ReturnNothing )

        MouseLeave position ->
            if (Debug.log "isInViewport" <| isInViewport model position) then
                ( model, ReturnNothing )
            else
                ( { model | maybeMouseOver = Nothing }, ReturnNothing )

        PrivateMouseClick position ->
            -- let
            --     _ =
            --         Debug.log "latlng" (viewportPointToLatLng model position)
            -- _ =
            --     Debug.log "position" position
            -- in
            ( model, OutMsg <| MouseClick position )

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
                ( newModel, ReturnNothing )

        ParentMsg parentMsg ->
            ( model, SelfMsg parentMsg )
