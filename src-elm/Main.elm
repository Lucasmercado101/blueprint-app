port module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Point
import Random
import Rect exposing (Point, Rectangle)
import Svg as S exposing (Svg, line, rect, svg)
import Svg.Attributes as SA exposing (fill, r, stroke, strokeWidth, version, viewBox, x1, x2, y1, y2)
import UUID exposing (UUID)


snapDistanceRange : Int
snapDistanceRange =
    20


snapMinValidDistance : Int
snapMinValidDistance =
    200



-- TODO: get dynamically


screenHeight : Int
screenHeight =
    800


screenWidth : Int
screenWidth =
    1900



-- PORTS


port requestGetSvgBoundingBox : String -> Cmd msg


port receiveGotSvgBoundingBox : (JD.Value -> msg) -> Sub msg


receiveChangedSvgTextContentDecoder :
    Decoder
        { updatedText : String
        , id : UUID
        , boundingBox :
            { x : Float
            , y : Float
            , width : Float
            , height : Float
            }
        }
receiveChangedSvgTextContentDecoder =
    JD.map3
        (\updated id rect ->
            ( updated, id, rect )
        )
        (JD.field "updatedText" JD.string)
        (JD.field "id" JD.string)
        (JD.field "boundingBox" bboxDecoder)
        |> JD.andThen
            (\( updated, id, rect ) ->
                case UUID.fromString id of
                    Ok uuid ->
                        JD.succeed
                            { updatedText = updated
                            , id = uuid
                            , boundingBox = rect
                            }

                    Err _ ->
                        JD.fail "Invalid UUID"
            )


bboxDecoder : Decoder { x : Float, y : Float, width : Float, height : Float }
bboxDecoder =
    JD.map4
        (\x y w h ->
            { x = x
            , y = y
            , width = w
            , height = h
            }
        )
        (JD.field "x" JD.float)
        (JD.field "y" JD.float)
        (JD.field "width" JD.float)
        (JD.field "height" JD.float)



-- SUBSCRIPTIONS


port changeSvgTextContent : ( String, String ) -> Cmd msg


port receiveChangedSvgTextContent : (JD.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveChangedSvgTextContent
        (\l ->
            case JD.decodeValue receiveChangedSvgTextContentDecoder l of
                Ok msg ->
                    Debug.todo "received text svg change"

                Err err ->
                    Debug.todo "received text svg change"
        )


mouseMoveDecoder : Decoder ( Int, Int )
mouseMoveDecoder =
    JD.map2 (\x y -> ( x, y ))
        (JD.field "layerX" JD.int)
        (JD.field "layerY" JD.int)



-- MODEL


type alias RoomID =
    UUID


type alias Line =
    ( Point, Point )


type alias Room =
    { id : RoomID
    , boundingBox : Rectangle
    }


type alias Model =
    { viewport : Point
    , mode : Mode
    , rooms : List Room
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { viewport = ( 0, 0 )
      , mode = Pan NotPanning
      , rooms = []
      }
    , Cmd.none
    )



-- UPDATE


type Mode
    = Pan PanState
    | Draw DrawState
    | Select
        { selected : SelectedRoom
        , state : HoveringOverOrDraggingRoom
        }
    | Delete


type PanState
    = NotPanning
    | Panning
        { panOrigin : Point
        , panEnd : Point
        }


type SelectedRoom
    = NoRoomSelected
    | RoomSelected RoomID
    | GroupSelected (List RoomID)


type HoveringOverOrDraggingRoom
    = NotHoveringOverRoom
    | HoldingClickOutsideAnyRooms {- `origin` and `end` are relative to viewport -}
    | DraggingToSelectMany
        { origin : Point
        , end : Point
        }
    | HoveringOverRoom RoomID
    | HoldingClickOnRoom RoomID
    | DraggingRoom
        { room : RoomID
        , dragOrigin : Point
        , dragEnd : Point
        , isOverlappingAnotherRoom : Bool
        }
    | DraggingRooms
        { rooms : List RoomID
        , dragOrigin : Point
        , dragEnd : Point
        , isOverlappingAnotherRoom : Bool
        }


type DrawState
    = NotDrawing
    | HoldingClick
    | Dragging
        { start : Point
        , end : Point
        , isOverlappingAnotherRoom : Bool
        }


type Msg
    = MouseMove Point
    | MouseDown Point
    | MouseUp Point
    | DrawMode
    | PanMode
    | DeleteMode
    | SelectMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        changeMode mode =
            { model | mode = mode }

        pure : a -> ( a, Cmd Msg )
        pure m =
            ( m, Cmd.none )

        ignore =
            pure model
    in
    case msg of
        DeleteMode ->
            changeMode Delete |> pure

        DrawMode ->
            changeMode (Draw NotDrawing) |> pure

        PanMode ->
            changeMode (Pan NotPanning) |> pure

        SelectMode ->
            changeMode (Select { selected = NoRoomSelected, state = NotHoveringOverRoom })
                |> pure

        MouseDown mouseDownRelCoords ->
            case model.mode of
                Delete ->
                    ignore

                Pan _ ->
                    changeMode (Pan (Panning { panOrigin = mouseDownRelCoords, panEnd = mouseDownRelCoords }))
                        |> pure

                Draw state ->
                    case state of
                        NotDrawing ->
                            changeMode (Draw HoldingClick) |> pure

                        HoldingClick ->
                            ignore

                        Dragging _ ->
                            ignore

                Select { selected } ->
                    let
                        sceneMouseDownCoords =
                            mouseDownRelCoords |> Point.add model.viewport

                        onARoom : Maybe RoomID
                        onARoom =
                            model.rooms
                                |> getFirstRoom (sceneMouseDownCoords |> isOnRoom)
                                |> Maybe.map .id
                    in
                    (case onARoom of
                        Just room ->
                            changeMode (Select { selected = selected, state = HoldingClickOnRoom room })

                        Nothing ->
                            changeMode (Select { selected = selected, state = HoldingClickOutsideAnyRooms })
                    )
                        |> pure

        MouseMove mouseMoveRelCoords ->
            case model.mode of
                Delete ->
                    ignore

                Pan state ->
                    case state of
                        NotPanning ->
                            ignore

                        Panning { panOrigin } ->
                            changeMode (Pan (Panning { panOrigin = panOrigin, panEnd = mouseMoveRelCoords }))
                                |> pure

                Draw state ->
                    case state of
                        NotDrawing ->
                            ignore

                        HoldingClick ->
                            let
                                sceneRectangle : Rectangle
                                sceneRectangle =
                                    pointsToRectangle mouseMoveRelCoords mouseMoveRelCoords
                                        |> Rect.addPosition model.viewport

                                isOverlappingAnotherRoom : Bool
                                isOverlappingAnotherRoom =
                                    model.rooms
                                        |> List.map .boundingBox
                                        |> List.any (Rect.isThereAnyOverlap sceneRectangle)
                            in
                            changeMode (Draw (Dragging { start = mouseMoveRelCoords, end = mouseMoveRelCoords, isOverlappingAnotherRoom = isOverlappingAnotherRoom }))
                                |> pure

                        Dragging { start } ->
                            let
                                sceneRectangle : Rectangle
                                sceneRectangle =
                                    pointsToRectangle start mouseMoveRelCoords
                                        |> Rect.addPosition model.viewport

                                isOverlappingAnotherRoom : Bool
                                isOverlappingAnotherRoom =
                                    model.rooms
                                        |> List.map .boundingBox
                                        |> List.any (Rect.isThereAnyOverlap sceneRectangle)
                            in
                            changeMode (Draw (Dragging { start = start, end = mouseMoveRelCoords, isOverlappingAnotherRoom = isOverlappingAnotherRoom }))
                                |> pure

                Select { selected, state } ->
                    let
                        sceneMouseMoveCoords =
                            mouseMoveRelCoords |> toGlobal model.viewport

                        roomImHoveringOver : Maybe RoomID
                        roomImHoveringOver =
                            model.rooms
                                |> getFirstRoom (sceneMouseMoveCoords |> isOnRoom)
                                |> Maybe.map .id
                    in
                    case state of
                        NotHoveringOverRoom ->
                            case roomImHoveringOver of
                                Just room ->
                                    changeMode (Select { selected = selected, state = HoveringOverRoom room })
                                        |> pure

                                Nothing ->
                                    ignore

                        ------------------------------------------
                        HoldingClickOutsideAnyRooms ->
                            changeMode
                                (Select
                                    { selected = selected
                                    , state =
                                        DraggingToSelectMany
                                            { origin = mouseMoveRelCoords
                                            , end = mouseMoveRelCoords
                                            }
                                    }
                                )
                                |> pure

                        DraggingToSelectMany { origin } ->
                            changeMode
                                (Select
                                    { selected = NoRoomSelected
                                    , state =
                                        DraggingToSelectMany
                                            { origin = origin
                                            , end = mouseMoveRelCoords
                                            }
                                    }
                                )
                                |> pure

                        DraggingRooms { rooms, dragOrigin, dragEnd } ->
                            let
                                excludingCurrentlySelectedRooms initialRooms =
                                    List.filter (\r -> not <| List.any (\e -> e == r.id) rooms) initialRooms

                                deltaDrag : Point
                                deltaDrag =
                                    dragOrigin |> Point.subtract dragEnd
                            in
                            changeMode
                                (Select
                                    { selected = selected
                                    , state =
                                        DraggingRooms
                                            { rooms = rooms
                                            , dragOrigin = dragOrigin
                                            , dragEnd = mouseMoveRelCoords |> toGlobal model.viewport
                                            , isOverlappingAnotherRoom =
                                                -- TODO: refactor so it's not double loop
                                                model.rooms
                                                    |> List.any
                                                        (\r ->
                                                            List.any
                                                                (\e ->
                                                                    if r.id == e then
                                                                        let
                                                                            draggingRect =
                                                                                r
                                                                                    |> roomAddPosition deltaDrag
                                                                                    |> roomSubPosition model.viewport

                                                                            isOverlappingAnotherRoom : Bool
                                                                            isOverlappingAnotherRoom =
                                                                                model.rooms
                                                                                    |> excludingCurrentlySelectedRooms
                                                                                    |> List.filter (.boundingBox >> Rect.isThereAnyOverlap draggingRect.boundingBox)
                                                                                    |> List.head
                                                                                    |> Maybe.map (always True)
                                                                                    |> Maybe.withDefault False
                                                                        in
                                                                        isOverlappingAnotherRoom

                                                                    else
                                                                        False
                                                                )
                                                                rooms
                                                        )
                                            }
                                    }
                                )
                                |> pure

                        ------------------------------------------
                        HoveringOverRoom _ ->
                            case roomImHoveringOver of
                                Just room ->
                                    changeMode (Select { selected = selected, state = HoveringOverRoom room })
                                        |> pure

                                Nothing ->
                                    changeMode (Select { selected = selected, state = NotHoveringOverRoom })
                                        |> pure

                        HoldingClickOnRoom room ->
                            let
                                draggingSingleRoom : ( Model, Cmd Msg )
                                draggingSingleRoom =
                                    changeMode
                                        (Select
                                            { selected = selected
                                            , state =
                                                DraggingRoom
                                                    { room = room
                                                    , dragOrigin = sceneMouseMoveCoords
                                                    , dragEnd = sceneMouseMoveCoords
                                                    , isOverlappingAnotherRoom = False
                                                    }
                                            }
                                        )
                                        |> pure
                            in
                            case selected of
                                GroupSelected rooms ->
                                    if List.member room rooms then
                                        changeMode
                                            (Select
                                                { selected = selected
                                                , state =
                                                    DraggingRooms
                                                        { rooms = rooms
                                                        , dragEnd = sceneMouseMoveCoords
                                                        , dragOrigin = sceneMouseMoveCoords
                                                        , isOverlappingAnotherRoom = False
                                                        }
                                                }
                                            )
                                            |> pure

                                    else
                                        draggingSingleRoom

                                RoomSelected _ ->
                                    draggingSingleRoom

                                NoRoomSelected ->
                                    draggingSingleRoom

                        DraggingRoom { room, dragOrigin, dragEnd } ->
                            let
                                deltaDrag : Point
                                deltaDrag =
                                    Point.subtract dragEnd dragOrigin
                            in
                            changeMode
                                (Select
                                    { selected = selected
                                    , state =
                                        DraggingRoom
                                            { room = room
                                            , dragOrigin = dragOrigin
                                            , dragEnd = sceneMouseMoveCoords
                                            , isOverlappingAnotherRoom =
                                                model.rooms
                                                    |> List.any
                                                        (\r ->
                                                            if r.id == room then
                                                                let
                                                                    newRectangle : Rectangle
                                                                    newRectangle =
                                                                        r
                                                                            |> roomAddPosition deltaDrag
                                                                            |> .boundingBox

                                                                    isOverlappingAnotherRoom : Bool
                                                                    isOverlappingAnotherRoom =
                                                                        model.rooms
                                                                            |> List.filter (\e -> e.id /= room)
                                                                            |> List.filter (.boundingBox >> Rect.isThereAnyOverlap newRectangle)
                                                                            |> (not << List.isEmpty)
                                                                in
                                                                isOverlappingAnotherRoom

                                                            else
                                                                False
                                                        )
                                            }
                                    }
                                )
                                |> pure

        MouseUp mouseUpRelCoords ->
            case model.mode of
                Pan state ->
                    case state of
                        NotPanning ->
                            ignore

                        Panning { panOrigin, panEnd } ->
                            let
                                deltaPan =
                                    panEnd |> Point.subtract panOrigin
                            in
                            { model
                                | viewport = model.viewport |> Point.add deltaPan
                                , mode = Pan NotPanning
                            }
                                |> pure

                Select { selected, state } ->
                    case state of
                        NotHoveringOverRoom ->
                            ignore

                        HoldingClickOutsideAnyRooms ->
                            changeMode (Select { selected = NoRoomSelected, state = NotHoveringOverRoom })
                                |> pure

                        DraggingToSelectMany { origin, end } ->
                            let
                                selectArea : Rectangle
                                selectArea =
                                    pointsToRectangle (origin |> Point.add model.viewport) (end |> Point.add model.viewport)

                                roomsSelected : List Room
                                roomsSelected =
                                    model.rooms
                                        |> List.filter (.boundingBox >> Rect.isInside selectArea)
                            in
                            changeMode
                                (Select
                                    { selected =
                                        if List.isEmpty roomsSelected then
                                            NoRoomSelected

                                        else
                                            case roomsSelected of
                                                [ room ] ->
                                                    RoomSelected room.id

                                                _ ->
                                                    GroupSelected (roomsSelected |> List.map .id)
                                    , state = NotHoveringOverRoom
                                    }
                                )
                                |> pure

                        HoveringOverRoom _ ->
                            ignore

                        HoldingClickOnRoom roomId ->
                            changeMode
                                (Select
                                    { selected =
                                        case selected of
                                            RoomSelected roomImEditing ->
                                                if roomImEditing == roomId then
                                                    NoRoomSelected

                                                else
                                                    RoomSelected roomId

                                            GroupSelected _ ->
                                                RoomSelected roomId

                                            NoRoomSelected ->
                                                RoomSelected roomId
                                    , state = HoveringOverRoom roomId
                                    }
                                )
                                |> pure

                        DraggingRoom { room, dragOrigin, dragEnd, isOverlappingAnotherRoom } ->
                            { model
                                | rooms =
                                    if isOverlappingAnotherRoom then
                                        model.rooms

                                    else
                                        model.rooms
                                            |> List.map
                                                (\r ->
                                                    if r.id == room then
                                                        let
                                                            deltaDrag : Point
                                                            deltaDrag =
                                                                dragOrigin |> Point.subtract dragEnd

                                                            newDraggedRoom =
                                                                r |> roomAddPosition deltaDrag

                                                            draggedRoomAfterSnapping : Room
                                                            draggedRoomAfterSnapping =
                                                                handleSnapping newDraggedRoom (model.rooms |> List.filter (\l -> l.id /= room))
                                                                    |> handleTranslateRoomToSnappedPosition newDraggedRoom
                                                        in
                                                        draggedRoomAfterSnapping

                                                    else
                                                        r
                                                )
                                , mode = Select { selected = selected, state = HoveringOverRoom room }
                            }
                                |> pure

                        DraggingRooms { rooms, dragOrigin, dragEnd } ->
                            let
                                deltaDrag =
                                    Point.subtract dragEnd dragOrigin

                                globalMouseUpCoords =
                                    mouseUpRelCoords |> toGlobal model.viewport

                                roomImHoveringOver : Maybe RoomID
                                roomImHoveringOver =
                                    model.rooms
                                        |> getFirstRoom (globalMouseUpCoords |> isOnRoom)
                                        |> Maybe.map .id

                                anySelectedRoomIsOverlappingARoom : Bool
                                anySelectedRoomIsOverlappingARoom =
                                    model.rooms
                                        |> List.any
                                            (\r ->
                                                if List.any (\e -> r.id == e) rooms then
                                                    let
                                                        newRectangle : Rectangle
                                                        newRectangle =
                                                            r
                                                                |> roomAddPosition deltaDrag
                                                                |> .boundingBox

                                                        isOverlappingAnotherRoom : Bool
                                                        isOverlappingAnotherRoom =
                                                            model.rooms
                                                                |> List.filter (\e -> not <| List.any (\l -> e.id == l) rooms)
                                                                |> List.filter (.boundingBox >> Rect.isThereAnyOverlap newRectangle)
                                                                |> (not << List.isEmpty)
                                                    in
                                                    if isOverlappingAnotherRoom then
                                                        True

                                                    else
                                                        False

                                                else
                                                    False
                                            )
                            in
                            { model
                                | rooms =
                                    if anySelectedRoomIsOverlappingARoom then
                                        model.rooms

                                    else
                                        model.rooms
                                            |> List.map
                                                (\r ->
                                                    if List.any (\e -> r.id == e) rooms then
                                                        r |> roomAddPosition deltaDrag

                                                    else
                                                        r
                                                )
                                , mode =
                                    Select
                                        { selected = selected
                                        , state =
                                            case roomImHoveringOver of
                                                Just r ->
                                                    HoveringOverRoom r

                                                Nothing ->
                                                    NotHoveringOverRoom
                                        }
                            }
                                |> pure

                Delete ->
                    let
                        sceneMouseUpCoords =
                            mouseUpRelCoords |> Point.add model.viewport
                    in
                    { model | rooms = model.rooms |> List.filter (sceneMouseUpCoords |> isNotOnRoom) }
                        |> pure

                Draw state ->
                    case state of
                        NotDrawing ->
                            ignore

                        HoldingClick ->
                            changeMode (Draw NotDrawing) |> pure

                        Dragging { start, end, isOverlappingAnotherRoom } ->
                            let
                                ( x1, y1 ) =
                                    start

                                ( x2, y2 ) =
                                    end

                                hasNoWidth =
                                    x1 == x2

                                hasNoHeight =
                                    y1 == y2
                            in
                            if isOverlappingAnotherRoom || hasNoWidth || hasNoHeight then
                                changeMode (Draw NotDrawing) |> pure

                            else
                                let
                                    ( ox, oy ) =
                                        model.viewport

                                    rectId : UUID
                                    rectId =
                                        Random.step UUID.generator (Random.initialSeed (x1 + y1 + x2 + y2 + ox + oy + 12345))
                                            |> Tuple.first

                                    sceneRect : Rectangle
                                    sceneRect =
                                        pointsToRectangle start end
                                            |> rectToGlobal model.viewport
                                in
                                { model
                                    | rooms =
                                        { boundingBox = sceneRect

                                        --   TODO: change to use random, have it be a command
                                        , id = rectId
                                        }
                                            :: model.rooms
                                    , mode = Draw NotDrawing
                                }
                                    |> pure


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    -- TODO: only draw rooms that are visible, inside the viewport
    div []
        [ div
            ([ style "background-color" background
             , style "width" "100vw"
             , style "height" "100vh"
             , style "position" "relative"

             --  TODO: drag by holding middle click and moving mouse
             --  , on "auxclick"
             ]
                ++ (case model.mode of
                        -- TODO: add highlight on hover on delete mode
                        Delete ->
                            [ on "mouseup" (mouseMoveDecoder |> JD.map MouseUp)
                            ]

                        Pan panState ->
                            let
                                isPanning =
                                    case panState of
                                        NotPanning ->
                                            False

                                        Panning _ ->
                                            True
                            in
                            [ on "mouseup" (mouseMoveDecoder |> JD.map MouseUp)
                            , if isPanning then
                                on "mousemove" (mouseMoveDecoder |> JD.map MouseMove)

                              else
                                style "" ""
                            , if isPanning then
                                style "cursor" "grabbing"

                              else
                                style "cursor" "grab"
                            , on "mousedown" (mouseMoveDecoder |> JD.map MouseDown)
                            ]

                        Draw state ->
                            [ on "mousedown" (mouseMoveDecoder |> JD.map MouseDown)
                            , case state of
                                NotDrawing ->
                                    style "" ""

                                HoldingClick ->
                                    on "mousemove" (mouseMoveDecoder |> JD.map MouseMove)

                                Dragging _ ->
                                    on "mousemove" (mouseMoveDecoder |> JD.map MouseMove)
                            , case state of
                                NotDrawing ->
                                    style "" ""

                                HoldingClick ->
                                    on "mouseup" (mouseMoveDecoder |> JD.map MouseUp)

                                Dragging _ ->
                                    on "mouseup" (mouseMoveDecoder |> JD.map MouseUp)
                            ]

                        Select _ ->
                            [ on "mousemove" (mouseMoveDecoder |> JD.map MouseMove)
                            , on "mousedown" (mouseMoveDecoder |> JD.map MouseDown)
                            , on "mouseup" (mouseMoveDecoder |> JD.map MouseUp)
                            ]
                   )
            )
            -- NOTE: Drawing order is bottom to top, draw last on top
            [ svg [ version "1.1", SA.width (screenWidth |> String.fromInt), SA.height (screenHeight |> String.fromInt), viewBox "0 0 1900 800" ]
                -- DRAW
                (let
                    viewport : Point
                    viewport =
                        case model.mode of
                            Pan state ->
                                case state of
                                    NotPanning ->
                                        model.viewport

                                    Panning { panOrigin, panEnd } ->
                                        let
                                            panDist =
                                                Point.subtract panOrigin panEnd
                                        in
                                        model.viewport |> Point.add panDist

                            _ ->
                                model.viewport

                    bgGrid : List (Svg Msg)
                    bgGrid =
                        backgroundGrid viewport

                    globalRectToRel : Rectangle -> Rectangle
                    globalRectToRel { x1, y1, width, height } =
                        let
                            rel : Point
                            rel =
                                toRelative ( x1, y1 ) viewport
                        in
                        { x1 = rel |> Point.x
                        , y1 = rel |> Point.y
                        , width = width
                        , height = height
                        }

                    drawRooms : List Room -> List (Svg Msg)
                    drawRooms rooms =
                        rooms
                            |> List.map
                                (\{ boundingBox } ->
                                    drawRect (globalRectToRel boundingBox)
                                        [ strokeWidth "2"
                                        , stroke "white"
                                        , fill "transparent"
                                        ]
                                )
                 in
                 -- NOTE: Drawing order is top to bottom, draw on top last
                 case model.mode of
                    Delete ->
                        bgGrid ++ drawRooms model.rooms

                    Pan _ ->
                        bgGrid ++ drawRooms model.rooms

                    Draw state ->
                        bgGrid
                            ++ (case state of
                                    NotDrawing ->
                                        drawRooms model.rooms

                                    HoldingClick ->
                                        drawRooms model.rooms

                                    Dragging { start, end, isOverlappingAnotherRoom } ->
                                        drawRooms model.rooms
                                            ++ [ drawRect (pointsToRectangle start end)
                                                    [ strokeWidth "2"
                                                    , stroke
                                                        (if isOverlappingAnotherRoom then
                                                            "red"

                                                         else
                                                            "white"
                                                        )
                                                    , fill "transparent"
                                                    ]
                                               ]
                               )

                    Select { selected, state } ->
                        let
                            noSelectedRooms : List Room -> List Room
                            noSelectedRooms initialRooms =
                                case selected of
                                    NoRoomSelected ->
                                        initialRooms

                                    RoomSelected room ->
                                        initialRooms |> List.filter (\r -> r.id /= room)

                                    GroupSelected rooms ->
                                        initialRooms
                                            |> List.filter (\r -> not <| List.member r.id rooms)

                            drawHighlightedRooms : List Room -> List (Svg Msg)
                            drawHighlightedRooms rooms =
                                rooms
                                    |> List.map
                                        (\{ boundingBox } ->
                                            drawRect (boundingBox |> globalRectToRel)
                                                [ strokeWidth "2"
                                                , stroke "white"
                                                , fill "rgba(255,255,255,0.1)"
                                                ]
                                        )

                            hoveredOverRoom : Maybe RoomID
                            hoveredOverRoom =
                                case state of
                                    HoveringOverRoom room ->
                                        Just room

                                    HoldingClickOnRoom room ->
                                        Just room

                                    _ ->
                                        Nothing

                            noHoveredOverRoom : List Room -> List Room
                            noHoveredOverRoom initialRooms =
                                case hoveredOverRoom of
                                    Just room ->
                                        initialRooms |> List.filter (\r -> r.id /= room)

                                    Nothing ->
                                        initialRooms

                            noDraggingRoom : List Room -> List Room
                            noDraggingRoom initialRooms =
                                case state of
                                    DraggingRoom { room } ->
                                        initialRooms |> List.filter (\r -> r.id /= room)

                                    _ ->
                                        initialRooms

                            drawRoomBeingDragged : List Room -> Svg Msg
                            drawRoomBeingDragged rooms =
                                case state of
                                    DraggingRoom { room, dragOrigin, dragEnd, isOverlappingAnotherRoom } ->
                                        rooms
                                            |> getFirstRoom (\{ id } -> id == room)
                                            |> Maybe.map
                                                (\roomImDragging ->
                                                    let
                                                        deltaDrag : Point
                                                        deltaDrag =
                                                            dragOrigin |> Point.subtract dragEnd

                                                        newDraggedRoom =
                                                            roomImDragging
                                                                |> roomAddPosition deltaDrag
                                                                |> roomSubPosition model.viewport

                                                        draggedRoomAfterSnapping : Room
                                                        draggedRoomAfterSnapping =
                                                            (model.rooms |> List.filter (\r -> r.id /= room) |> List.map (roomSubPosition model.viewport))
                                                                |> handleSnapping newDraggedRoom
                                                                |> handleTranslateRoomToSnappedPosition newDraggedRoom
                                                    in
                                                    drawRect
                                                        draggedRoomAfterSnapping.boundingBox
                                                        [ strokeWidth "2"
                                                        , stroke
                                                            (if isOverlappingAnotherRoom then
                                                                "red"

                                                             else
                                                                "white"
                                                            )
                                                        , fill
                                                            (if isOverlappingAnotherRoom then
                                                                "rgba(255,0,0,0.1)"

                                                             else
                                                                "rgba(255,255,255,0.1)"
                                                            )
                                                        ]
                                                )
                                            |> Maybe.withDefault (S.text "")

                                    _ ->
                                        none

                            drawRoomsBeingDragged : List Room -> List (Svg Msg)
                            drawRoomsBeingDragged initialRooms =
                                case state of
                                    DraggingRooms { rooms, dragOrigin, dragEnd, isOverlappingAnotherRoom } ->
                                        initialRooms
                                            |> List.filter (\{ id } -> List.member id rooms)
                                            |> List.map
                                                (\{ boundingBox } ->
                                                    let
                                                        deltaDrag : Point
                                                        deltaDrag =
                                                            Point.subtract dragEnd dragOrigin
                                                    in
                                                    drawRect
                                                        (boundingBox
                                                            |> Rect.addPosition deltaDrag
                                                            |> Rect.subPosition model.viewport
                                                        )
                                                        [ strokeWidth "2"
                                                        , stroke
                                                            (if isOverlappingAnotherRoom then
                                                                "red"

                                                             else
                                                                "white"
                                                            )
                                                        , fill
                                                            (if isOverlappingAnotherRoom then
                                                                "rgba(255,0,0,0.1)"

                                                             else
                                                                "rgba(255,255,255,0.1)"
                                                            )
                                                        ]
                                                )

                                    _ ->
                                        []

                            drawRoomBeingDraggedSnappingLines : List (Svg Msg)
                            drawRoomBeingDraggedSnappingLines =
                                case state of
                                    DraggingRoom { room, dragOrigin, dragEnd } ->
                                        model.rooms
                                            |> getFirstRoom (\{ id } -> id == room)
                                            |> Maybe.map
                                                (\roomImDragging ->
                                                    let
                                                        deltaDrag : Point
                                                        deltaDrag =
                                                            Point.subtract dragEnd dragOrigin

                                                        relDraggedRoom : Room
                                                        relDraggedRoom =
                                                            roomImDragging
                                                                |> roomAddPosition deltaDrag
                                                                |> roomSubPosition model.viewport

                                                        snappingPoints =
                                                            handleSnapping relDraggedRoom
                                                                (model.rooms
                                                                    |> List.filter (\r -> r.id /= room)
                                                                    |> List.map (roomSubPosition model.viewport)
                                                                )

                                                        draggedRoomAfterSnapping : Rectangle
                                                        draggedRoomAfterSnapping =
                                                            handleTranslateRoomToSnappedPosition relDraggedRoom snappingPoints
                                                                |> (\e -> e.boundingBox)

                                                        -- TODO: not drawing lines properly if a is inside b but i am if b is inside a
                                                        -- check snap middle middle on a room that's bigger inside a room that's smaller
                                                        -- on both cases
                                                        drawHorizontalLines : ( RoomPossibleSnappingX, RoomPossibleSnappingX, Room ) -> List (Svg msg)
                                                        drawHorizontalLines ( currRoomSnapKind, otherRoomSnapKind, roomSnapped ) =
                                                            case ( currRoomSnapKind, otherRoomSnapKind ) of
                                                                ( SnappingXTop, SnappingXTop ) ->
                                                                    let
                                                                        x1 =
                                                                            min roomSnapped.boundingBox.x1 draggedRoomAfterSnapping.x1

                                                                        y1 =
                                                                            min roomSnapped.boundingBox.y1 draggedRoomAfterSnapping.y1

                                                                        x2 =
                                                                            max (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width) (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width)

                                                                        y2 =
                                                                            max draggedRoomAfterSnapping.y1 roomSnapped.boundingBox.y1
                                                                    in
                                                                    line
                                                                        [ SA.x1 (x1 |> toString)
                                                                        , SA.y1 (y1 |> toString)
                                                                        , SA.x2 (x2 |> toString)
                                                                        , SA.y2 (y2 |> toString)
                                                                        , stroke "orange"
                                                                        , strokeWidth "2"
                                                                        ]
                                                                        []
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingXTop, SnappingXMiddle ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1)
                                                                            && (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width <= roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width)
                                                                     then
                                                                        --  is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1 then
                                                                        -- is on the right
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the left
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingXTop, SnappingXBottom ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1)
                                                                            && (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width <= roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1 then
                                                                        -- is on the right
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the left
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingXMiddle, SnappingXTop ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1)
                                                                            && (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width <= roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1 then
                                                                        -- is on the right
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the left
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingXMiddle, SnappingXMiddle ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1)
                                                                            && (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width <= roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1 then
                                                                        -- is on the right
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the left
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingXMiddle, SnappingXBottom ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1)
                                                                            && (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width <= roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1 then
                                                                        -- is on the right
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the left
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 + (draggedRoomAfterSnapping.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingXBottom, SnappingXTop ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1)
                                                                            && (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width <= roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1 then
                                                                        -- is on the right
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the left
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingXBottom, SnappingXMiddle ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1)
                                                                            && (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width <= roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1 then
                                                                        -- is on the right
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the left
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 + (roomSnapped.boundingBox.height // 2) ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingXBottom, SnappingXBottom ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1)
                                                                            && (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width <= roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.x1 <= draggedRoomAfterSnapping.x1 then
                                                                        -- is on the right
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the left
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]

                                                        drawVerticalLines : ( RoomPossibleSnappingY, RoomPossibleSnappingY, Room ) -> List (Svg msg)
                                                        drawVerticalLines ( currRoomSnapKind, otherRoomSnapKind, roomSnapped ) =
                                                            case ( currRoomSnapKind, otherRoomSnapKind ) of
                                                                ( SnappingYLeft, SnappingYLeft ) ->
                                                                    let
                                                                        x1 =
                                                                            min roomSnapped.boundingBox.x1 draggedRoomAfterSnapping.x1

                                                                        y1 =
                                                                            min roomSnapped.boundingBox.y1 draggedRoomAfterSnapping.y1

                                                                        x2 =
                                                                            max draggedRoomAfterSnapping.x1 roomSnapped.boundingBox.x1

                                                                        y2 =
                                                                            max (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height) (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height)
                                                                    in
                                                                    line
                                                                        [ SA.x1 (x1 |> toString)
                                                                        , SA.y1 (y1 |> toString)
                                                                        , SA.x2 (x2 |> toString)
                                                                        , SA.y2 (y2 |> toString)
                                                                        , stroke "orange"
                                                                        , strokeWidth "2"
                                                                        ]
                                                                        []
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingYLeft, SnappingYMiddle ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1)
                                                                            && (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height <= roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height)
                                                                     then
                                                                        --  is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1 then
                                                                        -- is on the bottom
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the top
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2), roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2), roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingYLeft, SnappingYRight ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1)
                                                                            && (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height <= roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1 then
                                                                        -- is on the bottom
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the top
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingYMiddle, SnappingYLeft ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1)
                                                                            && (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height <= roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1 then
                                                                        -- is on the bottom
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2) |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the top
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2) |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2), draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2), draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingYMiddle, SnappingYMiddle ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1)
                                                                            && (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height <= roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1 then
                                                                        -- is on the bottom
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2) |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the top
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2) |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2), roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2), roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2), draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2), draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingYMiddle, SnappingYRight ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1)
                                                                            && (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height <= roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1 then
                                                                        -- is on the bottom
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2) |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the top
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2) |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2), draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + (draggedRoomAfterSnapping.width // 2), draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingYRight, SnappingYLeft ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1)
                                                                            && (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height <= roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1 then
                                                                        -- is on the bottom
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the top
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingYRight, SnappingYMiddle ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1)
                                                                            && (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height <= roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1 then
                                                                        -- is on the bottom
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the top
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2) |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2), roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + (roomSnapped.boundingBox.width // 2), roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]

                                                                ( SnappingYRight, SnappingYRight ) ->
                                                                    (if
                                                                        (roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1)
                                                                            && (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height <= roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height)
                                                                     then
                                                                        -- is inside
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else if roomSnapped.boundingBox.y1 <= draggedRoomAfterSnapping.y1 then
                                                                        -- is on the bottom
                                                                        line
                                                                            [ SA.x1 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y1 (roomSnapped.boundingBox.y1 |> toString)
                                                                            , SA.x2 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y2 (draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []

                                                                     else
                                                                        -- is on the top
                                                                        line
                                                                            [ SA.x1 (draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width |> toString)
                                                                            , SA.y1 (draggedRoomAfterSnapping.y1 |> toString)
                                                                            , SA.x2 (roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width |> toString)
                                                                            , SA.y2 (roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height |> toString)
                                                                            , stroke "orange"
                                                                            , strokeWidth "2"
                                                                            ]
                                                                            []
                                                                    )
                                                                        :: drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( roomSnapped.boundingBox.x1 + roomSnapped.boundingBox.width, roomSnapped.boundingBox.y1 + roomSnapped.boundingBox.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 ) 5 [ stroke "orange", strokeWidth "2" ]
                                                                        ++ drawX ( draggedRoomAfterSnapping.x1 + draggedRoomAfterSnapping.width, draggedRoomAfterSnapping.y1 + draggedRoomAfterSnapping.height ) 5 [ stroke "orange", strokeWidth "2" ]
                                                    in
                                                    case snappingPoints of
                                                        -- TODO: vertically draw fn
                                                        ( Just snappedHorizontally, Just snappedVertically ) ->
                                                            drawHorizontalLines snappedHorizontally ++ drawVerticalLines snappedVertically

                                                        ( Just snappedHorizontally, Nothing ) ->
                                                            drawHorizontalLines snappedHorizontally

                                                        ( Nothing, Just snappedVertically ) ->
                                                            drawVerticalLines snappedVertically

                                                        ( Nothing, Nothing ) ->
                                                            []
                                                )
                                            |> Maybe.withDefault []

                                    _ ->
                                        [ none ]

                            noGroupBeingHighlightedRooms : List Room -> List Room
                            noGroupBeingHighlightedRooms initialRooms =
                                case selected of
                                    GroupSelected rooms ->
                                        initialRooms |> List.filter (\r -> not <| List.member r.id rooms)

                                    _ ->
                                        case state of
                                            DraggingToSelectMany { origin, end } ->
                                                let
                                                    draggingArea =
                                                        pointsToRectangle (origin |> toGlobal viewport) (end |> toGlobal viewport)
                                                in
                                                initialRooms |> List.filter (\r -> not <| Rect.isInside draggingArea r.boundingBox)

                                            _ ->
                                                initialRooms

                            drawSelectionArea : List (Svg Msg)
                            drawSelectionArea =
                                case state of
                                    DraggingToSelectMany { origin, end } ->
                                        [ drawRect
                                            (pointsToRectangle origin end)
                                            [ strokeWidth "2"

                                            -- TODO: colors could be nicer
                                            , stroke "rgba(87,121,0,0.5)"
                                            , fill "rgba(36,49,4,0.5)"
                                            ]
                                        ]

                                    _ ->
                                        []

                            noRoomsBeingDragged : List Room -> List Room
                            noRoomsBeingDragged initialRooms =
                                case state of
                                    DraggingRooms { rooms } ->
                                        initialRooms
                                            |> List.filter (\r -> not <| List.member r.id rooms)

                                    _ ->
                                        initialRooms
                        in
                        bgGrid
                            ++ drawRooms
                                (model.rooms
                                    |> noGroupBeingHighlightedRooms
                                    |> noSelectedRooms
                                    |> noHoveredOverRoom
                                    |> noDraggingRoom
                                    |> noRoomsBeingDragged
                                )
                            ++ drawHighlightedRooms
                                (model.rooms
                                    |> List.filter
                                        (\room ->
                                            not <|
                                                List.member room.id
                                                    (model.rooms
                                                        |> noSelectedRooms
                                                        |> noHoveredOverRoom
                                                        |> noGroupBeingHighlightedRooms
                                                        |> List.map .id
                                                    )
                                        )
                                    |> noRoomsBeingDragged
                                    |> noDraggingRoom
                                )
                            ++ drawRoomBeingDragged model.rooms
                            :: drawSelectionArea
                            ++ drawRoomsBeingDragged model.rooms
                            ++ drawRoomBeingDraggedSnappingLines
                )
            ]

        -- change modes
        , div
            [ style "position" "absolute"
            , style
                "left"
                "50%"
            , style "bottom" "0"
            , style "transform" "translate(-50%, 0)"
            , style "background-color" "white"
            , style "padding" "15px"
            , style "display" "flex"
            , style "gap" "15px"
            ]
            [ button [ style "padding" "5px", onClick PanMode ] [ text "Pan" ]
            , button [ style "padding" "5px", onClick DrawMode ] [ text "Draw" ]
            , button [ style "padding" "5px", onClick SelectMode ] [ text "Select" ]
            , button [ style "padding" "5px", onClick DeleteMode ] [ text "Delete" ]
            ]
        ]


drawSnappingLines : Point -> Line -> Line -> List (Svg Msg)
drawSnappingLines globalViewPanOffset firstLine secondLine =
    let
        ( firstLineStart, firstLineEnd ) =
            firstLine

        ( secondLineStart, secondLineEnd ) =
            secondLine

        ( x1, y1 ) =
            tupleSub firstLineStart globalViewPanOffset

        ( x2, y2 ) =
            tupleSub secondLineStart globalViewPanOffset

        ( x3, y3 ) =
            tupleSub firstLineEnd globalViewPanOffset

        ( x4, y4 ) =
            tupleSub secondLineEnd globalViewPanOffset
    in
    line
        [ SA.x1 (x1 |> String.fromInt)
        , SA.y1 (y1 |> String.fromInt)
        , SA.x2 (x4 |> String.fromInt)
        , SA.y2 (y4 |> String.fromInt)
        , stroke "orange"
        , strokeWidth "2"
        ]
        []
        :: drawX ( x1, y1 ) 5 [ stroke "orange", strokeWidth "2" ]
        ++ drawX ( x2, y2 ) 5 [ stroke "orange", strokeWidth "2" ]
        ++ drawX ( x3, y3 ) 5 [ stroke "orange", strokeWidth "2" ]
        ++ drawX ( x4, y4 ) 5 [ stroke "orange", strokeWidth "2" ]


drawX : Point -> Int -> List (S.Attribute msg) -> List (Svg msg)
drawX point size attrs =
    let
        ( x, y ) =
            point
    in
    [ line
        ([ SA.x1 (x - size |> String.fromInt)
         , SA.y1 (y - size |> String.fromInt)
         , SA.x2 (x + size |> String.fromInt)
         , SA.y2 (y + size |> String.fromInt)
         ]
            ++ attrs
        )
        []
    , line
        ([ SA.x1 (x - size |> String.fromInt)
         , SA.y1 (y + size |> String.fromInt)
         , SA.x2 (x + size |> String.fromInt)
         , SA.y2 (y - size |> String.fromInt)
         ]
            ++ attrs
        )
        []
    ]



{-
   Drawing relative to the viewport
-}


drawRect : Rectangle -> List (Attribute Msg) -> Svg Msg
drawRect rectangle attrs =
    let
        { x1, y1, width, height } =
            rectangle
    in
    rect
        ([ SA.x (x1 |> toString)
         , SA.y (y1 |> toString)
         , SA.height (height |> toString)
         , SA.width (width |> toString)
         ]
            ++ attrs
        )
        []


drawRectanglePoint : Point -> Rectangle -> Svg Msg
drawRectanglePoint globalView { x1, y1, width, height } =
    let
        ( gx, gy ) =
            globalView
    in
    S.text_
        [ SA.x (x1 - gx |> String.fromInt)
        , SA.y (y1 - gy - 10 |> String.fromInt)
        , SA.class "svgText"
        , SA.fill "white"
        ]
        [ S.text
            ("Rel X: "
                ++ (x1 - gx |> String.fromInt)
                ++ " Rel Y: "
                ++ (y1 - gy |> String.fromInt)
                ++ " W: "
                ++ (width |> String.fromInt)
                ++ " H: "
                ++ (height |> String.fromInt)
                ++ " GlobalX: "
                ++ (x1 |> String.fromInt)
                ++ " GlobalY: "
                ++ (y1 |> String.fromInt)
            )
        ]



-- infinitely repeating grid


backgroundGrid : Point -> List (Svg Msg)
backgroundGrid ( gx, gy ) =
    let
        size =
            50

        ( x, y ) =
            ( gx |> modBy size, gy |> modBy size )
    in
    (List.range 0 100
        |> List.map
            (\i ->
                S.line
                    -- TODO: use dynamic screen resolution
                    [ x1 (size * i - x - (size * 2) |> String.fromInt)
                    , y1 (0 - y |> String.fromInt)
                    , x2 (size * i - x - (size * 2) |> String.fromInt)
                    , y2 (1200 - y |> String.fromInt)
                    , SA.stroke "white"
                    , SA.strokeWidth "1"
                    , SA.strokeDasharray "5,5"
                    , SA.opacity "0.5"
                    ]
                    []
            )
    )
        ++ (List.range 0 100
                |> List.map
                    (\i ->
                        S.line
                            [ x1 (0 - x |> String.fromInt)
                            , y1 (size * i - y - (size * 2) |> String.fromInt)
                            , x2 (1900 - y + (size * 2) |> String.fromInt)
                            , y2 (size * i - y - (size * 2) |> String.fromInt)
                            , SA.stroke "white"
                            , SA.strokeWidth "1"
                            , SA.strokeDasharray "5,5"
                            , SA.opacity "0.5"
                            ]
                            []
                    )
           )



-- OTHER


background : String
background =
    "#023770"


toGlobal : Point -> Point -> Point
toGlobal relativeToOffset offsetPan =
    let
        ( gx, gy ) =
            offsetPan

        ( x, y ) =
            relativeToOffset
    in
    ( x + gx, y + gy )


toRelative : Point -> Point -> Point
toRelative relativeToOffset offsetPan =
    let
        ( gx, gy ) =
            offsetPan

        ( x, y ) =
            relativeToOffset
    in
    ( x - gx, y - gy )


tupleToString : ( Int, Int ) -> ( String, String )
tupleToString ( x, y ) =
    ( String.fromInt x, String.fromInt y )


tupleSub : ( number, number ) -> ( number, number ) -> ( number, number )
tupleSub ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


tupleAdd : ( number, number ) -> ( number, number ) -> ( number, number )
tupleAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )



{- b - range <= a <= b + range -}


numWithinRange : number -> number -> number -> Bool
numWithinRange a b range =
    b - range <= a && a <= b + range


nbsp : String
nbsp =
    "\u{00A0}"


getFirstRoom : (Room -> Bool) -> List Room -> Maybe Room
getFirstRoom predicate rooms =
    rooms
        |> List.foldl
            (\room acc ->
                case acc of
                    Just _ ->
                        acc

                    Nothing ->
                        if predicate room then
                            Just room

                        else
                            Nothing
            )
            Nothing


getIdOfRoomBeingHoveredOrDragged : HoveringOverOrDraggingRoom -> Maybe RoomID
getIdOfRoomBeingHoveredOrDragged hoveringOverOrDraggingRoom =
    case hoveringOverOrDraggingRoom of
        NotHoveringOverRoom ->
            Nothing

        HoveringOverRoom id ->
            Just id

        HoldingClickOnRoom id ->
            Just id

        DraggingRoom { room } ->
            Just room

        DraggingRooms _ ->
            Nothing

        HoldingClickOutsideAnyRooms ->
            Nothing

        DraggingToSelectMany _ ->
            Nothing


calcSlope : ( Float, Float ) -> ( Float, Float ) -> Float
calcSlope ( x1, y1 ) ( x2, y2 ) =
    (y2 - y1) / (x2 - x1)


none : Svg msg
none =
    S.text ""


isOnRoom : Point -> Room -> Bool
isOnRoom point room =
    Rect.isPointOnRectangle point room.boundingBox


isNotOnRoom : Point -> Room -> Bool
isNotOnRoom point room =
    not <| isOnRoom point room


pointsToRectangle : Point -> Point -> Rectangle
pointsToRectangle ( x1, y1 ) ( x2, y2 ) =
    let
        xStart =
            min x1 x2

        yStart =
            min y1 y2

        xEnd =
            max x1 x2

        yEnd =
            max y1 y2
    in
    { x1 = xStart
    , y1 = yStart
    , width = xEnd - xStart
    , height = yEnd - yStart
    }


rectToGlobal : Point -> Rectangle -> Rectangle
rectToGlobal globalView { x1, y1, width, height } =
    let
        ( gx, gy ) =
            globalView
    in
    { x1 = x1 + gx
    , y1 = y1 + gy
    , width = width
    , height = height
    }


closestRoomX : Room -> List Room -> Maybe Room
closestRoomX room rooms =
    let
        centerX =
            room.boundingBox.x1 + (room.boundingBox.height // 2)
    in
    case rooms of
        [] ->
            Nothing

        [ onlyRoom ] ->
            Just onlyRoom

        x :: xs ->
            Just
                (List.foldl
                    (\next current ->
                        let
                            centerXCurrent =
                                current.boundingBox.x1 + (current.boundingBox.height // 2)

                            centerXNext =
                                next.boundingBox.x1 + (next.boundingBox.height // 2)

                            dNext =
                                abs (centerX - centerXNext)

                            dCurr =
                                abs (centerX - centerXCurrent)
                        in
                        if dNext < dCurr then
                            next

                        else
                            current
                    )
                    x
                    xs
                )


closestRoomY : Room -> List Room -> Maybe Room
closestRoomY room rooms =
    let
        centerY =
            room.boundingBox.y1 + (room.boundingBox.width // 2)
    in
    case rooms of
        [] ->
            Nothing

        [ onlyRoom ] ->
            Just onlyRoom

        x :: xs ->
            Just
                (List.foldl
                    (\next current ->
                        let
                            centerYCurrent =
                                current.boundingBox.y1 + (current.boundingBox.width // 2)

                            centerYNext =
                                next.boundingBox.y1 + (next.boundingBox.width // 2)

                            dNext =
                                abs (centerY - centerYNext)

                            dCurr =
                                abs (centerY - centerYCurrent)
                        in
                        if dNext < dCurr then
                            next

                        else
                            current
                    )
                    x
                    xs
                )


type RoomPossibleSnappingX
    = SnappingXTop
    | SnappingXMiddle
    | SnappingXBottom


type RoomPossibleSnappingY
    = SnappingYLeft
    | SnappingYMiddle
    | SnappingYRight


inRange : Int -> Int -> Int -> Bool
inRange min max number =
    min <= number && number <= max



-- TODO: refactor to allow reuse
-- i want to get other sides that it snaps to
-- so, after snapping, get new position after translating it to the new snapped position
-- then re-get a matching room, ignoring the side that i already snapped to, and then do it again
-- to get the possible remaining snapped sides/center.
-- should be as simple as just adding an "if" on the -whereToSnap functions
-- TODO: refactor so that the valid area for snapping is just a rectangle
-- instead of two lines that validate x and y separately
-- as visually they end up as a rectangle anyways, might be easier to code/read/reason about


handleSnapping : Room -> List Room -> ( Maybe ( RoomPossibleSnappingX, RoomPossibleSnappingX, Room ), Maybe ( RoomPossibleSnappingY, RoomPossibleSnappingY, Room ) )
handleSnapping roomToSnap allRooms =
    let
        isOnValidSnappableYRange : Room -> Room -> Bool
        isOnValidSnappableYRange a b =
            let
                a1 =
                    a.boundingBox.y1

                a2 =
                    a.boundingBox.y1 + a.boundingBox.height

                b1 =
                    b.boundingBox.y1 - snapDistanceRange

                b2 =
                    b.boundingBox.y1 + b.boundingBox.height + snapDistanceRange
            in
            overlap1DLines ( a1, a2 ) ( b1, b2 )

        isValidXDistanceClose : Room -> Room -> Bool
        isValidXDistanceClose a b =
            let
                a1 =
                    a.boundingBox.x1

                a2 =
                    a.boundingBox.x1 + a.boundingBox.width

                b1 =
                    b.boundingBox.x1 - snapMinValidDistance

                b2 =
                    b.boundingBox.x1 + b.boundingBox.width + snapMinValidDistance
            in
            overlap1DLines ( a1, a2 ) ( b1, b2 )

        isOnValidSnappableXRange : Room -> Room -> Bool
        isOnValidSnappableXRange a b =
            let
                a1 =
                    a.boundingBox.x1

                a2 =
                    a.boundingBox.x1 + a.boundingBox.width

                b1 =
                    b.boundingBox.x1 - snapDistanceRange

                b2 =
                    b.boundingBox.x1 + b.boundingBox.width + snapDistanceRange
            in
            overlap1DLines ( a1, a2 ) ( b1, b2 )

        isValidYDistanceClose : Room -> Room -> Bool
        isValidYDistanceClose a b =
            let
                a1 =
                    a.boundingBox.y1

                a2 =
                    a.boundingBox.y1 + a.boundingBox.height

                b1 =
                    b.boundingBox.y1 - snapMinValidDistance

                b2 =
                    b.boundingBox.y1 + b.boundingBox.height + snapMinValidDistance
            in
            overlap1DLines ( a1, a2 ) ( b1, b2 )

        addRoom r e =
            Maybe.map (\( f, s ) -> ( f, s, r )) e
    in
    List.foldl
        (\currRoom acc ->
            let
                currentRoomIsSnappingVertically =
                    if (currRoom |> isOnValidSnappableXRange roomToSnap) && (currRoom |> isValidYDistanceClose roomToSnap) then
                        whereToSnapVertically roomToSnap currRoom |> addRoom currRoom

                    else
                        Nothing

                currentRoomIsSnappingHorizontally =
                    if (currRoom |> isOnValidSnappableYRange roomToSnap) && (currRoom |> isValidXDistanceClose roomToSnap) then
                        whereToSnapHorizontally roomToSnap currRoom |> addRoom currRoom

                    else
                        Nothing

                isCloserXRoom : Room -> Bool
                isCloserXRoom currentlyMatched =
                    let
                        centerX =
                            roomToSnap.boundingBox |> Rect.centerX

                        currentCenterX =
                            currentlyMatched.boundingBox |> Rect.centerX

                        centerXNext =
                            currRoom.boundingBox |> Rect.centerX

                        dNext =
                            abs (centerX - centerXNext)

                        dCurr =
                            abs (centerX - currentCenterX)
                    in
                    if dNext < dCurr then
                        True

                    else
                        False

                isCloserYRoom : Room -> Bool
                isCloserYRoom currentlyMatched =
                    let
                        centerY =
                            roomToSnap.boundingBox |> Rect.centerY

                        currentCenterY =
                            currentlyMatched.boundingBox |> Rect.centerY

                        centerYNext =
                            currRoom.boundingBox |> Rect.centerY

                        dNext =
                            abs (centerY - centerYNext)

                        dCurr =
                            abs (centerY - currentCenterY)
                    in
                    if dNext < dCurr then
                        True

                    else
                        False
            in
            case acc of
                ( Nothing, Nothing ) ->
                    ( currentRoomIsSnappingHorizontally
                    , currentRoomIsSnappingVertically
                    )

                ( Just (( _, _, previousRoomMatched ) as previousHorizontalMatch), Nothing ) ->
                    ( case currentRoomIsSnappingHorizontally of
                        Just ( a, b, _ ) ->
                            if isCloserXRoom previousRoomMatched then
                                Just ( a, b, currRoom )

                            else
                                Just previousHorizontalMatch

                        Nothing ->
                            Just previousHorizontalMatch
                    , currentRoomIsSnappingVertically
                    )

                ( Nothing, Just (( _, _, previousRoomMatched ) as previousVerticalMatch) ) ->
                    ( currentRoomIsSnappingHorizontally
                    , case currentRoomIsSnappingVertically of
                        Just ( c, d, _ ) ->
                            if isCloserYRoom previousRoomMatched then
                                Just ( c, d, currRoom )

                            else
                                Just previousVerticalMatch

                        Nothing ->
                            Just previousVerticalMatch
                    )

                ( Just (( _, _, previousHRoomMatched ) as previousHorizontalMatch), Just (( _, _, previousVRoomMatched ) as previousVerticalMatch) ) ->
                    ( case currentRoomIsSnappingHorizontally of
                        Just ( a, b, _ ) ->
                            if isCloserXRoom previousHRoomMatched then
                                Just ( a, b, currRoom )

                            else
                                Just previousHorizontalMatch

                        Nothing ->
                            Just previousHorizontalMatch
                    , case currentRoomIsSnappingVertically of
                        Just ( c, d, _ ) ->
                            if isCloserYRoom previousVRoomMatched then
                                Just ( c, d, currRoom )

                            else
                                Just previousVerticalMatch

                        Nothing ->
                            Just previousVerticalMatch
                    )
        )
        ( Nothing, Nothing )
        allRooms


whereToSnapHorizontally : Room -> Room -> Maybe ( RoomPossibleSnappingX, RoomPossibleSnappingX )
whereToSnapHorizontally room roomImChecking =
    let
        topY =
            room.boundingBox.y1

        centerY =
            Rect.centerY room.boundingBox

        bottomY =
            Rect.bottomY room.boundingBox

        isInsideTopSnappableArea : Int -> Bool
        isInsideTopSnappableArea number =
            inRange (number - snapDistanceRange) (number + snapDistanceRange) roomImChecking.boundingBox.y1

        insideTopDistance : Int -> Int
        insideTopDistance number =
            abs (number - roomImChecking.boundingBox.y1)

        isInsideMiddleSnappableArea : Int -> Bool
        isInsideMiddleSnappableArea number =
            inRange (number - snapDistanceRange) (number + snapDistanceRange) (Rect.centerY roomImChecking.boundingBox)

        insideMiddleDistance : Int -> Int
        insideMiddleDistance number =
            abs (number - Rect.centerY roomImChecking.boundingBox)

        isInsideBottomSnappableArea : Int -> Bool
        isInsideBottomSnappableArea number =
            inRange (number - snapDistanceRange) (number + snapDistanceRange) (Rect.bottomY roomImChecking.boundingBox)

        insideBottomDistance : Int -> Int
        insideBottomDistance number =
            abs (number - Rect.bottomY roomImChecking.boundingBox)

        tt =
            if topY |> isInsideTopSnappableArea then
                Just ( SnappingXTop, SnappingXTop, insideTopDistance topY )

            else
                Nothing

        mt =
            if centerY |> isInsideTopSnappableArea then
                Just ( SnappingXMiddle, SnappingXTop, insideTopDistance centerY )

            else
                Nothing

        bt =
            if bottomY |> isInsideTopSnappableArea then
                Just ( SnappingXBottom, SnappingXTop, insideTopDistance bottomY )

            else
                Nothing

        tm =
            if topY |> isInsideMiddleSnappableArea then
                Just ( SnappingXTop, SnappingXMiddle, insideMiddleDistance topY )

            else
                Nothing

        mm =
            if centerY |> isInsideMiddleSnappableArea then
                Just ( SnappingXMiddle, SnappingXMiddle, insideMiddleDistance centerY )

            else
                Nothing

        bm =
            if bottomY |> isInsideMiddleSnappableArea then
                Just ( SnappingXBottom, SnappingXMiddle, insideMiddleDistance bottomY )

            else
                Nothing

        tb =
            if topY |> isInsideBottomSnappableArea then
                Just ( SnappingXTop, SnappingXBottom, insideBottomDistance topY )

            else
                Nothing

        mb =
            if centerY |> isInsideBottomSnappableArea then
                Just ( SnappingXMiddle, SnappingXBottom, insideBottomDistance centerY )

            else
                Nothing

        bb =
            if bottomY |> isInsideBottomSnappableArea then
                Just ( SnappingXBottom, SnappingXBottom, insideBottomDistance bottomY )

            else
                Nothing
    in
    List.foldl
        (\next acc ->
            case acc of
                Just (( _, _, dist ) as snappingPairCurr) ->
                    case next of
                        Just (( _, _, distNext ) as snappingPairNext) ->
                            if distNext < dist then
                                Just snappingPairNext

                            else
                                Just snappingPairCurr

                        Nothing ->
                            acc

                Nothing ->
                    next
        )
        Nothing
        [ tt, mt, bt, tm, mm, bm, tb, mb, bb ]
        |> Maybe.map (\( a, b, _ ) -> ( a, b ))


whereToSnapVertically : Room -> Room -> Maybe ( RoomPossibleSnappingY, RoomPossibleSnappingY )
whereToSnapVertically room roomImChecking =
    let
        leftX =
            room.boundingBox.x1

        centerX =
            Rect.centerX room.boundingBox

        rightX =
            Rect.rightX room.boundingBox

        isInsideLeftSnappableArea : Int -> Bool
        isInsideLeftSnappableArea number =
            inRange (number - snapDistanceRange) (number + snapDistanceRange) roomImChecking.boundingBox.x1

        insideLeftDistance : Int -> Int
        insideLeftDistance number =
            abs (number - roomImChecking.boundingBox.x1)

        isInsideMiddleSnappableArea : Int -> Bool
        isInsideMiddleSnappableArea number =
            inRange (number - snapDistanceRange) (number + snapDistanceRange) (Rect.centerX roomImChecking.boundingBox)

        insideMiddleDistance : Int -> Int
        insideMiddleDistance number =
            abs (number - Rect.centerX roomImChecking.boundingBox)

        isInsideRightSnappableArea : Int -> Bool
        isInsideRightSnappableArea number =
            inRange (number - snapDistanceRange) (number + snapDistanceRange) (Rect.rightX roomImChecking.boundingBox)

        insideRightDistance : Int -> Int
        insideRightDistance number =
            abs (number - Rect.rightX roomImChecking.boundingBox)

        lr =
            if leftX |> isInsideRightSnappableArea then
                Just ( SnappingYLeft, SnappingYRight, insideRightDistance leftX )

            else
                Nothing

        mr =
            if centerX |> isInsideRightSnappableArea then
                Just ( SnappingYMiddle, SnappingYRight, insideRightDistance centerX )

            else
                Nothing

        rr =
            if rightX |> isInsideRightSnappableArea then
                Just ( SnappingYRight, SnappingYRight, insideRightDistance rightX )

            else
                Nothing

        lm =
            if leftX |> isInsideMiddleSnappableArea then
                Just ( SnappingYLeft, SnappingYMiddle, insideMiddleDistance leftX )

            else
                Nothing

        mm =
            if centerX |> isInsideMiddleSnappableArea then
                Just ( SnappingYMiddle, SnappingYMiddle, insideMiddleDistance centerX )

            else
                Nothing

        rm =
            if rightX |> isInsideMiddleSnappableArea then
                Just ( SnappingYRight, SnappingYMiddle, insideMiddleDistance rightX )

            else
                Nothing

        ll =
            if leftX |> isInsideLeftSnappableArea then
                Just ( SnappingYLeft, SnappingYLeft, insideLeftDistance leftX )

            else
                Nothing

        ml =
            if centerX |> isInsideLeftSnappableArea then
                Just ( SnappingYMiddle, SnappingYLeft, insideLeftDistance centerX )

            else
                Nothing

        rl =
            if rightX |> isInsideLeftSnappableArea then
                Just ( SnappingYRight, SnappingYLeft, insideLeftDistance rightX )

            else
                Nothing
    in
    List.foldl
        (\next acc ->
            case acc of
                Just (( _, _, dist ) as snappingPairCurr) ->
                    case next of
                        Just (( _, _, distNext ) as snappingPairNext) ->
                            if distNext < dist then
                                Just snappingPairNext

                            else
                                Just snappingPairCurr

                        Nothing ->
                            acc

                Nothing ->
                    next
        )
        Nothing
        [ lr, mr, rr, lm, mm, rm, ll, ml, rl ]
        |> Maybe.map (\( a, b, _ ) -> ( a, b ))


overlap1DLines : ( Int, Int ) -> ( Int, Int ) -> Bool
overlap1DLines ( a1, a2 ) ( b1, b2 ) =
    inRange a1 a2 b1 || inRange a1 a2 b2 || inRange b1 b2 a1 || inRange b1 b2 a2


isInside1DLine : ( Int, Int ) -> ( Int, Int ) -> Bool
isInside1DLine ( a1, a2 ) ( b1, b2 ) =
    (b1 |> inRange a1 a2) && (b2 |> inRange a1 a2)


translateRoomToSnappedPosition : Maybe ( RoomPossibleSnappingX, RoomPossibleSnappingX, Room ) -> Maybe ( RoomPossibleSnappingY, RoomPossibleSnappingY, Room ) -> Room -> Room
translateRoomToSnappedPosition horizontalSnap verticalSnap roomToTranslate =
    (case horizontalSnap of
        Just ( xFirstSnapKind, xSnapSecondKind, xRoomSnappedTo ) ->
            let
                toTranslateBoundingBox =
                    roomToTranslate.boundingBox

                xSnappedBBox =
                    xRoomSnappedTo.boundingBox
            in
            (case ( xFirstSnapKind, xSnapSecondKind ) of
                ( SnappingXTop, SnappingXTop ) ->
                    xSnappedBBox.y1

                ( SnappingXTop, SnappingXMiddle ) ->
                    Rect.centerY xSnappedBBox

                ( SnappingXTop, SnappingXBottom ) ->
                    Rect.bottomY xSnappedBBox

                ( SnappingXMiddle, SnappingXTop ) ->
                    xSnappedBBox.y1 - (toTranslateBoundingBox.height // 2)

                ( SnappingXMiddle, SnappingXMiddle ) ->
                    Rect.centerY xSnappedBBox - (toTranslateBoundingBox.height // 2)

                ( SnappingXMiddle, SnappingXBottom ) ->
                    Rect.bottomY xSnappedBBox - (toTranslateBoundingBox.height // 2)

                ( SnappingXBottom, SnappingXTop ) ->
                    xSnappedBBox.y1 - toTranslateBoundingBox.height

                ( SnappingXBottom, SnappingXMiddle ) ->
                    Rect.centerY xSnappedBBox - toTranslateBoundingBox.height

                ( SnappingXBottom, SnappingXBottom ) ->
                    Rect.bottomY xSnappedBBox - toTranslateBoundingBox.height
            )
                |> (\l -> { roomToTranslate | boundingBox = { toTranslateBoundingBox | y1 = l } })

        Nothing ->
            roomToTranslate
    )
        |> (\horizontallySnappedRoom ->
                case verticalSnap of
                    Just ( yFirstSnapKind, ySnapSecondKind, yRoomSnappedTo ) ->
                        let
                            toTranslateBoundingBox =
                                horizontallySnappedRoom.boundingBox

                            ySnappedBBox =
                                yRoomSnappedTo.boundingBox
                        in
                        (case ( yFirstSnapKind, ySnapSecondKind ) of
                            ( SnappingYLeft, SnappingYLeft ) ->
                                ySnappedBBox.x1

                            ( SnappingYLeft, SnappingYMiddle ) ->
                                Rect.centerX ySnappedBBox

                            ( SnappingYLeft, SnappingYRight ) ->
                                Rect.rightX ySnappedBBox

                            ( SnappingYMiddle, SnappingYLeft ) ->
                                ySnappedBBox.x1 - (toTranslateBoundingBox.width // 2)

                            ( SnappingYMiddle, SnappingYMiddle ) ->
                                Rect.centerX ySnappedBBox - (toTranslateBoundingBox.width // 2)

                            ( SnappingYMiddle, SnappingYRight ) ->
                                Rect.rightX ySnappedBBox - (toTranslateBoundingBox.width // 2)

                            ( SnappingYRight, SnappingYLeft ) ->
                                ySnappedBBox.x1 - toTranslateBoundingBox.width

                            ( SnappingYRight, SnappingYMiddle ) ->
                                Rect.centerX ySnappedBBox - toTranslateBoundingBox.width

                            ( SnappingYRight, SnappingYRight ) ->
                                Rect.rightX ySnappedBBox - toTranslateBoundingBox.width
                        )
                            |> (\l -> { horizontallySnappedRoom | boundingBox = { toTranslateBoundingBox | x1 = l } })

                    Nothing ->
                        horizontallySnappedRoom
           )


handleTranslateRoomToSnappedPosition : Room -> ( Maybe ( RoomPossibleSnappingX, RoomPossibleSnappingX, Room ), Maybe ( RoomPossibleSnappingY, RoomPossibleSnappingY, Room ) ) -> Room
handleTranslateRoomToSnappedPosition roomToTranslate snapKinds =
    case snapKinds of
        ( Just xSnap, Just ySnap ) ->
            translateRoomToSnappedPosition (Just xSnap) (Just ySnap) roomToTranslate

        ( Nothing, Just ySnap ) ->
            translateRoomToSnappedPosition Nothing (Just ySnap) roomToTranslate

        ( Just xSnap, Nothing ) ->
            translateRoomToSnappedPosition (Just xSnap) Nothing roomToTranslate

        ( Nothing, Nothing ) ->
            roomToTranslate


pointsToLine : Int -> Int -> ( Int, Int )
pointsToLine x1 x2 =
    ( min x1 x2, max x1 x2 )


roomAddPosition : Point -> Room -> Room
roomAddPosition ( x, y ) room =
    let
        bBox =
            room.boundingBox
    in
    { room
        | boundingBox =
            { bBox
                | x1 = bBox.x1 + x
                , y1 = bBox.y1 + y
            }
    }


roomSubPosition : Point -> Room -> Room
roomSubPosition ( x, y ) room =
    let
        bBox =
            room.boundingBox
    in
    { room
        | boundingBox =
            { bBox
                | x1 = bBox.x1 - x
                , y1 = bBox.y1 - y
            }
    }


getTotalRoomsXSpaceSorted : List Room -> List Rectangle
getTotalRoomsXSpaceSorted rooms =
    let
        isInsideFound1DLines : List ( Int, Int ) -> ( Int, Int ) -> Bool
        isInsideFound1DLines lines line =
            List.any (\l -> line |> isInside1DLine l) lines

        isThereATopmostRoom : Maybe Room
        isThereATopmostRoom =
            rooms |> List.sortBy (.boundingBox >> .y1) |> List.head
    in
    case isThereATopmostRoom of
        Nothing ->
            []

        Just topmostRoom ->
            let
                getNextTopmostRoom : List Room -> List ( Int, Int ) -> Maybe Room
                getNextTopmostRoom roomsToSearch lines =
                    roomsToSearch
                        |> List.foldl
                            (\next acc ->
                                let
                                    nextRect : Rectangle
                                    nextRect =
                                        next.boundingBox

                                    nextRoomTopXLine =
                                        ( nextRect |> Rect.topLeft |> Point.x, nextRect |> Rect.topRight |> Point.x )
                                in
                                case acc of
                                    Nothing ->
                                        if nextRoomTopXLine |> isInsideFound1DLines lines then
                                            Nothing

                                        else
                                            Just next

                                    Just curr ->
                                        let
                                            currRect =
                                                curr.boundingBox
                                        in
                                        if nextRoomTopXLine |> isInsideFound1DLines lines then
                                            Just curr

                                        else if (nextRect |> Rect.topLeft |> Point.y) < (currRect |> Rect.topLeft |> Point.y) then
                                            Just next

                                        else
                                            Just curr
                            )
                            Nothing

                getAllTopmostRooms : List Room -> List Room -> List Room
                getAllTopmostRooms allRooms currentTopmostRooms =
                    let
                        topXline rect =
                            ( rect |> Rect.topLeft |> Point.x, rect |> Rect.topRight |> Point.x )
                    in
                    case getNextTopmostRoom allRooms (currentTopmostRooms |> List.map (.boundingBox >> topXline)) of
                        Nothing ->
                            currentTopmostRooms

                        Just nextTopmost ->
                            let
                                nextTopmostRooms =
                                    nextTopmost :: currentTopmostRooms

                                allRoomsMinusTopMostRooms =
                                    allRooms |> List.filter (\r -> not <| List.any (\r2 -> r.id == r2.id) nextTopmostRooms)
                            in
                            getAllTopmostRooms allRoomsMinusTopMostRooms nextTopmostRooms
            in
            getAllTopmostRooms rooms [ topmostRoom ] |> List.map .boundingBox
