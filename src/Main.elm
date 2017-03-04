module Main exposing (main)

import Geo
    exposing
        ( LatLng
        , slippyTileUrl
        , latLngToSlippyTileNumber
        , SlippyTileNumber
        , latLngToWorldPixelPoint
        , worldPixelPointToSlippyTileNumber
        , getTileTopLeftWorldPixelPoint
        )
import VectorMath exposing (Vector2DInt, Point2DInt, difference, negate)
import Constants exposing (slippyTileSize)
import Html exposing (Html, beginnerProgram, img, input, div, text)
import Html.Attributes exposing (src, type_, value, style, draggable)
import Html.Events exposing (onInput, onMouseDown, onMouseUp, onMouseLeave, on)
import Util exposing (cartesianProduct)
import Maybe.Extra


-- import DOM
-- import Json.Decode as Json

import Mouse
import MouseEvents exposing (relPos, onMouseEnter)


initLatLng : LatLng
initLatLng =
    { lat = -37.813611, lng = 144.963056 }


initZoom : Int
initZoom =
    4



-- { lat = -85.0, lng = 0.0 }
-- { lat = 85.0, lng = 0.0 }


type alias Model =
    { latLng : LatLng
    , zoom : Int
    , mapWidthPx : Int
    , mapHeightPx : Int
    , maybeMouseOver : Maybe MouseOverState
    }


getDraggingOffset : MouseOverState -> Maybe Vector2DInt
getDraggingOffset mouseOverState =
    mouseOverState.down
        |> Maybe.map
            (.startPosition
                >> difference mouseOverState.position
                >> VectorMath.negate
            )


type alias MouseOverState =
    { position : Mouse.Position, down : Maybe { startPosition : Mouse.Position } }


model : Model
model =
    { latLng = initLatLng
    , zoom = initZoom
    , mapWidthPx = 1000
    , mapHeightPx = 700
    , maybeMouseOver = Nothing
    }


type Msg
    = UpdateLat String
    | UpdateLng String
    | UpdateZoom String
    | MouseDown Mouse.Position
    | MouseUp
    | MouseMove Mouse.Position
    | MouseEnter Mouse.Position
    | MouseLeave


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
                maybeMouseOver =
                    model.maybeMouseOver
                        |> Maybe.map
                            (\mouseOverState -> { mouseOverState | down = Nothing })
            in
                { model | maybeMouseOver = maybeMouseOver } ! []

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



-- let
--     maybeMouseOver =
--         model.maybeMouseOver
--             |> Maybe.map (\({ position } as mouseOverState) -> { mouseOverState | down = True })
--             |> Maybe.withDefault
-- in
--     { model | maybeMouseOver = maybeMouseOver } ! []
-- MouseUp ->


view : Model -> Html Msg
view model =
    let
        tileNumber : SlippyTileNumber
        tileNumber =
            latLngToSlippyTileNumber model.zoom model.latLng

        url : String
        url =
            Debug.log "url" (slippyTileUrl "a" (Debug.log "tileCoords" tileNumber))
    in
        Html.div []
            [ mapViewportView model
            , input [ type_ "number", onInput UpdateLat, value <| toString model.latLng.lat ] []
            , input [ type_ "number", onInput UpdateLng, value <| toString model.latLng.lng ] []
            , input [ type_ "number", value <| toString model.zoom, onInput UpdateZoom ] []
            , div [] [ text <| "mouse state: " ++ toString model.maybeMouseOver ]
            ]


type alias TileViewModel =
    { tileNumber : SlippyTileNumber, viewportPoint : Point2DInt }


getTileViewModels : Model -> List TileViewModel
getTileViewModels model =
    let
        mapCenter =
            Debug.log "mapCenter" <|
                latLngToWorldPixelPoint model.zoom model.latLng

        worldWidthInTiles =
            2 ^ model.zoom

        viewportTopLeftWorldPixel =
            Debug.log "viewportTopLeftWorldPixel" <|
                { x = mapCenter.x - (model.mapWidthPx // 2)
                , y = mapCenter.y - (model.mapHeightPx // 2)
                , zoom = model.zoom
                }

        viewportBottomRightWorldPixel =
            Debug.log "viewportBottomRightWorldPixel" <|
                { x = mapCenter.x + (model.mapWidthPx // 2)
                , y = mapCenter.y + (model.mapHeightPx // 2)
                , zoom = model.zoom
                }

        topLeftTileNumber =
            Debug.log "topLeftTileNuber" <|
                worldPixelPointToSlippyTileNumber viewportTopLeftWorldPixel

        topLeftTileWorldPixelPoint =
            Debug.log "topLeftTileWOrldPixelPoint" <|
                getTileTopLeftWorldPixelPoint topLeftTileNumber

        topLeftTileViewportPoint =
            Debug.log "topLeftTileViewportPoint" <|
                { x = topLeftTileWorldPixelPoint.x - viewportTopLeftWorldPixel.x
                , y = topLeftTileWorldPixelPoint.y - viewportTopLeftWorldPixel.y
                }

        numColumns =
            Debug.log "numColumns" <|
                (viewportBottomRightWorldPixel.x - topLeftTileWorldPixelPoint.x)
                    // slippyTileSize
                    + 1

        numRows =
            Debug.log "numRows" <|
                (viewportBottomRightWorldPixel.y - topLeftTileWorldPixelPoint.y)
                    // slippyTileSize
                    + 1

        matrix : List ( Int, Int )
        matrix =
            Debug.log "matrix" <|
                cartesianProduct (List.range 0 (numColumns - 1)) (List.range 0 (numRows - 1))

        toTileViewModel ( viewportTileX, viewportTileY ) =
            let
                -- we really want to use translation functions here? Maybe use opensolid/geometry?
                currTileTopLeftViewportPoint =
                    { x = topLeftTileViewportPoint.x + (viewportTileX * slippyTileSize)
                    , y = topLeftTileViewportPoint.y + (viewportTileY * slippyTileSize)
                    }

                currTileBottomRightViewportPoint =
                    { x = currTileTopLeftViewportPoint.x + slippyTileSize
                    , y = currTileTopLeftViewportPoint.y + slippyTileSize
                    }
            in
                { tileNumber =
                    { x = (topLeftTileNumber.x + viewportTileX) % worldWidthInTiles
                    , y = (topLeftTileNumber.y + viewportTileY) % worldWidthInTiles
                    , zoom = topLeftTileNumber.zoom
                    }
                , viewportPoint = currTileTopLeftViewportPoint
                }

        -- we shouldn't keep zoom around for each tile (no need!) - it just needs to be set right at the top
    in
        List.map toTileViewModel matrix



-- doColumn
--     -- new approach:
--     -- just figure out x and y bounds ahead of time (easy!)
--     -- then generate the x y positions of all tiles (this will be a func funcitonal programmy thingo)
--     -- then just map over this. No need for recursion.
--     doRow
--     { currViewportTileX = 0, currViewportTileY = 0, tileViewModels = [] }
--     |> .tileViewModels


tileView : { model : Model, offset : Point2DInt } -> TileViewModel -> Html msg
tileView { model, offset } tileViewModel =
    let
        threeD =
            True

        threeD =
            False

        style_ =
            [ ( "position", "absolute" )
            , ( "left", (toString tileViewModel.viewportPoint.x) ++ "px" )
            , ( "top", (toString tileViewModel.viewportPoint.y) ++ "px" )
            , ( "pointerEvents", "None" )
            , ( "transform", transform )
            ]

        transform =
            "translate3d(" ++ toString offset.x ++ "px," ++ toString offset.y ++ "px, 0)"

        url =
            slippyTileUrl "a" tileViewModel.tileNumber
    in
        img [ draggable "false", src url, style style_ ] []


mapViewportView : Model -> Html Msg
mapViewportView model =
    let
        tileViewModels =
            getTileViewModels model

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
        div
            [ MouseEvents.onMouseDown (MouseEvents.relPos >> MouseDown)
            , MouseEvents.onMouseMove (MouseEvents.relPos >> MouseMove)
            , onMouseUp MouseUp
            , onMouseLeave MouseLeave
            , MouseEvents.onMouseEnter (MouseEvents.relPos >> MouseEnter)
              -- , on "mousemove" (DOM.target DOM.offsetWidth |> Json.map MouseMoved)
            , style
                [ ( "position", "relative" )
                , ( "overflow", "hidden" )
                , ( "width", (toString model.mapWidthPx) ++ "px" )
                , ( "height", (toString model.mapHeightPx) ++ "px" )
                ]
            ]
            -- Html Float myButton = button [ on "click" (target offsetWidth) ] [ text "Click me!" ]
            (List.map (tileView { model = model, offset = tileOffset }) tileViewModels)



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


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }
