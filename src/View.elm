module View exposing (..)

import Html exposing (Html, div, img, input, text)
import Html.Attributes exposing (draggable, src, style, type_, value)
import Html.Events exposing (onInput, onMouseLeave, onMouseUp)
import Html.Keyed
import Maybe.Extra
import Model exposing (getDraggingOffset)
import ViewModel exposing (getTileViewModels, TileViewModel)
import MouseEvents
import MouseWheel
import SlippyTiles exposing (SlippyTileNumber, latLngToSlippyTileNumber, slippyTileUrl)
import Types exposing (..)
import VectorMath exposing (Point2DInt, Vector2DInt)


view : Model -> Html Msg
view model =
    let
        tileNumber : SlippyTileNumber
        tileNumber =
            latLngToSlippyTileNumber model.zoom model.latLng

        url : String
        url =
            -- Debug.log "url" <|
            slippyTileUrl "a" tileNumber
    in
        Html.div []
            [ mapViewportView model
            , input [ type_ "number", onInput UpdateLat, value <| toString model.latLng.lat ] []
            , input [ type_ "number", onInput UpdateLng, value <| toString model.latLng.lng ] []
            , input [ type_ "number", value <| toString model.zoom, onInput UpdateZoom ] []
            , div [] [ text <| "mouse state: " ++ toString model.maybeMouseOver ]
            ]


mapViewportView : Model -> Html Msg
mapViewportView model =
    let
        tileViewModels =
            getTileViewModels model model.resolution

        lowResTileViewModels =
            getTileViewModels model (model.resolution / 2.0)

        lowResTileViewModels2 =
            getTileViewModels model (model.resolution / 4.0)

        tileOffset : Vector2DInt
        tileOffset =
            model.maybeMouseOver
                |> Maybe.map getDraggingOffset
                |> Maybe.Extra.join
                |> Maybe.withDefault { x = 0, y = 0 }

        -- this is a legitimate withDefault. We don't want to offset the tiles
        -- if the map is not being dragged
        -- so what we want to do is
        -- if dragging, then get the drag offset and apply that to all images
        -- using translate2D, and use withDefault to set the offset to { x = 0, y = 0}
    in
        Html.Keyed.node "div"
            [ MouseEvents.onMouseDown (MouseEvents.relPos >> MouseDown)
            , MouseEvents.onMouseMove (MouseEvents.relPos >> MouseMove)
            , onMouseUp MouseUp
            , MouseEvents.onMouseLeave (MouseEvents.relPos >> MouseLeave)
            , MouseEvents.onMouseEnter (MouseEvents.relPos >> MouseEnter)
            , MouseEvents.onClick (MouseEvents.relPos >> MouseClick)
            , MouseWheel.onMouseWheel (MouseWheel)
              -- , on "mousemove" (DOM.target DOM.offsetWidth |> Json.map MouseMoved)
            , style
                [ ( "position", "relative" )
                , ( "overflow", "hidden" )
                , ( "width", (toString model.mapWidthPx) ++ "px" )
                , ( "height", (toString model.mapHeightPx) ++ "px" )
                , ( "background", "url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAYAAACNMs+9AAAAJUlEQVQoU2N88eLFfwY0ICEhwYguxjgUFKI7GsTH5m4M3w1ChQCnziae7MntdQAAAABJRU5ErkJggg==) repeat" )
                ]
            ]
            -- Html Float myButton = button [ on "click" (target offsetWidth) ] [ text "Click me!" ]
            ((List.map (tileView { model = model, offset = tileOffset, z = 10 }) tileViewModels)
                ++ (List.map (tileView { model = model, offset = tileOffset, z = 2 }) lowResTileViewModels)
                ++ (List.map (tileView { model = model, offset = tileOffset, z = 1 }) lowResTileViewModels2)
            )



-- doColumn
--     -- new approach:
--     -- just figure out x and y bounds ahead of time (easy!)
--     -- then generate the x y positions of all tiles (this will be a func funcitonal programmy thingo)
--     -- then just map over this. No need for recursion.
--     doRow
--     { currViewportTileX = 0, currViewportTileY = 0, tileViewModels = [] }
--     |> .tileViewModels


tileView : { model : Model, offset : Point2DInt, z : Int } -> TileViewModel -> ( String, Html msg )
tileView { model, offset, z } tileViewModel =
    let
        threeD =
            True

        -- threeD =
        --     False
        style_ =
            [ ( "position", "absolute" )
            , ( "left", (toString tileViewModel.viewportPoint.x) ++ "px" )
            , ( "top", (toString tileViewModel.viewportPoint.y) ++ "px" )
            , ( "pointerEvents", "None" )
            , ( "transform", transform )
            , ( "width", (toString tileViewModel.tileSize) ++ "px" )
            , ( "height", (toString tileViewModel.tileSize) ++ "px" )
            , ( "zIndex", toString z )
            ]

        transform =
            "translate3d(" ++ toString offset.x ++ "px," ++ toString offset.y ++ "px, 0)"

        url =
            slippyTileUrl "a" tileViewModel.tileNumber
    in
        ( tileViewModel.tileNumber |> tileNumberToHtmlKey
        , img [ draggable "false", src url, style style_ ] []
        )


tileNumberToHtmlKey : SlippyTileNumber -> String
tileNumberToHtmlKey tile =
    [ tile.x |> toString
    , tile.y |> toString
    , tile.zoom |> toString
    ]
        |> String.join "-"



-- maybe to start we could just get the top row working, then worry about doing the rows? (so y is always 0)
-- currWorldPixel =
-- tiles =
-- Let's just do the top row (guess the y value)
-- We get the tile at the top left.
-- then we just contintue until the right x coord of the tile is greater than the width of the map,
-- keeping an array of (SlippyTileNumber, mapTopLeft
-- theres no real need to put it in a 2D array (this is purely for display)
-- when rendering, we just absolutely position the image and give them top and left values.
-- Done!
