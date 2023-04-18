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
{- Svg UUID as String since that's the id's name -}


port requestGetSvgBoundingBox : String -> Cmd msg



{- Svg UUID as String since that's the id's name -}


port receiveGotSvgBoundingBox : (JD.Value -> msg) -> Sub msg


receiveGotSvgBoundingBoxDecoder : Decoder ( String, Rectangle )
receiveGotSvgBoundingBoxDecoder =
    JD.map2 (\id rect -> ( id, rect ))
        (JD.field "id" JD.string)
        (JD.field "boundingBox" bboxDecoder)


bboxDecoder : Decoder Rectangle
bboxDecoder =
    JD.map4
        (\x y w h ->
            { x1 = x |> round
            , y1 = y |> round
            , width = w |> round
            , height = h |> round
            }
        )
        (JD.field "x" JD.float)
        (JD.field "y" JD.float)
        (JD.field "width" JD.float)
        (JD.field "height" JD.float)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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
    , snappingPointsLine : Maybe ( Line, Line )
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { viewport = ( 0, 0 )
      , mode = Pan NotPanning
      , rooms = []
      , snappingPointsLine = Nothing
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
        , sidesSnapping :
            { bottom : Maybe Room }
        }
    | DraggingRooms
        { rooms : List RoomID
        , dragOrigin : Point
        , dragEnd : Point
        , isOverlappingAnotherRoom : Bool
        }


type DrawState
    = NotDrawing
      -- NOTE: "relativeStartingPoint" is there so that
      -- the rectangle's start is always smaller than its end at the end of the day
    | SelectedStart
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
    case msg of
        DeleteMode ->
            ( { model
                | mode = Delete
              }
            , Cmd.none
            )

        DrawMode ->
            ( { model | mode = Draw NotDrawing }, Cmd.none )

        PanMode ->
            ( { model | mode = Pan NotPanning }, Cmd.none )

        SelectMode ->
            ( { model
                | mode =
                    Select
                        { selected = NoRoomSelected
                        , state = NotHoveringOverRoom
                        }
              }
            , Cmd.none
            )

        MouseDown mouseDownRelCoords ->
            case model.mode of
                Delete ->
                    ( model, Cmd.none )

                Pan _ ->
                    ( { model | mode = Pan (Panning { panOrigin = mouseDownRelCoords, panEnd = mouseDownRelCoords }) }, Cmd.none )

                Draw state ->
                    case state of
                        NotDrawing ->
                            ( { model
                                | mode =
                                    Draw
                                        (SelectedStart
                                            { start = mouseDownRelCoords
                                            , end = mouseDownRelCoords
                                            , isOverlappingAnotherRoom = False
                                            }
                                        )
                                , snappingPointsLine = Nothing
                              }
                            , Cmd.none
                            )

                        SelectedStart { start, end, isOverlappingAnotherRoom } ->
                            if isOverlappingAnotherRoom then
                                ( { model
                                    | mode = Draw NotDrawing
                                    , snappingPointsLine = Nothing
                                  }
                                , Cmd.none
                                )

                            else
                                let
                                    ( x1, y1 ) =
                                        start

                                    ( x2, y2 ) =
                                        end

                                    ( ox, oy ) =
                                        model.viewport

                                    rectId =
                                        Random.step UUID.generator (Random.initialSeed (x1 + y1 + x2 + y2 + ox + oy + 12345))
                                            |> Tuple.first
                                in
                                ( { model
                                    | rooms =
                                        { boundingBox =
                                            pointsToRectangle start end
                                                |> rectToGlobal model.viewport

                                        --   TODO: change to use random, have it be a command
                                        , id = rectId
                                        }
                                            :: model.rooms
                                    , mode = Draw NotDrawing
                                    , snappingPointsLine = Nothing
                                  }
                                  -- NOTE: not fetching the bounding box here because it's not in the dom yet
                                , Cmd.none
                                )

                Select { selected } ->
                    let
                        globalMouseDownCoords =
                            mouseDownRelCoords |> toGlobal model.viewport

                        onARoom : Maybe RoomID
                        onARoom =
                            model.rooms
                                |> getFirstRoom (globalMouseDownCoords |> isOnRoom)
                                |> Maybe.map .id
                    in
                    case onARoom of
                        Just room ->
                            ( { model | mode = Select { selected = selected, state = HoldingClickOnRoom room } }, Cmd.none )

                        Nothing ->
                            ( { model | mode = Select { selected = selected, state = HoldingClickOutsideAnyRooms } }, Cmd.none )

        MouseMove mouseMoveRelCoords ->
            case model.mode of
                Delete ->
                    ( model, Cmd.none )

                Pan state ->
                    case state of
                        NotPanning ->
                            ( model, Cmd.none )

                        Panning { panOrigin } ->
                            ( { model | mode = Pan (Panning { panOrigin = panOrigin, panEnd = mouseMoveRelCoords }) }, Cmd.none )

                Draw state ->
                    case state of
                        NotDrawing ->
                            ( model, Cmd.none )

                        SelectedStart { start, end } ->
                            -- TODO: fix weird bug where it stays snapped after starting to draw near a bottomY of another rectangle
                            let
                                drawingRect =
                                    pointsToRectangle start mouseMoveRelCoords
                                        |> rectToGlobal model.viewport

                                isOverlappingAnotherRoom : Bool
                                isOverlappingAnotherRoom =
                                    model.rooms
                                        |> List.map .boundingBox
                                        |> List.any (Rect.isThereAnyOverlap drawingRect)

                                bottomSideIsAlignedToAnotherRectangle : Maybe ( Line, Line )
                                bottomSideIsAlignedToAnotherRectangle =
                                    model.rooms
                                        |> List.map .boundingBox
                                        |> List.filter (\rect -> numWithinRange (rect |> Rect.bottomY) (drawingRect |> Rect.bottomY) snapDistanceRange)
                                        |> Rect.closestRectangleX drawingRect
                                        |> Maybe.andThen
                                            (\rect ->
                                                let
                                                    bl =
                                                        rect |> Rect.bottomLeft |> Point.x

                                                    br =
                                                        rect |> Rect.bottomRight |> Point.x

                                                    tlx =
                                                        drawingRect |> Rect.topLeft |> Point.x

                                                    brx =
                                                        drawingRect |> Rect.bottomRight |> Point.x

                                                    leftDist =
                                                        abs tlx - abs br

                                                    rightDist =
                                                        abs bl - abs brx

                                                    isOnTheRight =
                                                        if leftDist < rightDist then
                                                            True

                                                        else
                                                            False

                                                    minDistBeforeSnapping =
                                                        snapMinValidDistance
                                                in
                                                if isOnTheRight then
                                                    if rightDist <= minDistBeforeSnapping then
                                                        Just
                                                            ( ( drawingRect |> Rect.bottomLeft |> Tuple.mapSecond (always (rect |> Rect.bottomY))
                                                              , drawingRect |> Rect.bottomRight |> Tuple.mapSecond (always (rect |> Rect.bottomY))
                                                              )
                                                            , rect |> Rect.bottomSide
                                                            )

                                                    else
                                                        Nothing

                                                else if leftDist <= minDistBeforeSnapping then
                                                    Just
                                                        ( rect |> Rect.bottomSide
                                                        , ( drawingRect |> Rect.bottomLeft |> Tuple.mapSecond (always (rect |> Rect.bottomY))
                                                          , drawingRect |> Rect.bottomRight |> Tuple.mapSecond (always (rect |> Rect.bottomY))
                                                          )
                                                        )

                                                else
                                                    Nothing
                                            )

                                snapBottomPosition : Maybe { start : Point, end : Point }
                                snapBottomPosition =
                                    bottomSideIsAlignedToAnotherRectangle
                                        |> Maybe.map
                                            (\( ( l, _ ), _ ) ->
                                                toRelative l model.viewport |> Point.y
                                            )
                                        |> Maybe.map
                                            (\e ->
                                                { start = start
                                                , end = end |> Tuple.mapSecond (always e)
                                                }
                                            )
                            in
                            ( { model
                                | mode =
                                    Draw
                                        (SelectedStart
                                            (case snapBottomPosition of
                                                Just p ->
                                                    let
                                                        ( x1, _ ) =
                                                            mouseMoveRelCoords

                                                        ( _, y2 ) =
                                                            p.end
                                                    in
                                                    { start = p.start
                                                    , end = ( x1, y2 )
                                                    , isOverlappingAnotherRoom = isOverlappingAnotherRoom
                                                    }

                                                Nothing ->
                                                    { start = start
                                                    , end = mouseMoveRelCoords
                                                    , isOverlappingAnotherRoom = isOverlappingAnotherRoom
                                                    }
                                            )
                                        )
                                , snappingPointsLine = bottomSideIsAlignedToAnotherRectangle
                              }
                            , Cmd.none
                            )

                Select { selected, state } ->
                    let
                        mouseMoveGlobalCoords =
                            mouseMoveRelCoords |> toGlobal model.viewport

                        roomImHoveringOver : Maybe RoomID
                        roomImHoveringOver =
                            model.rooms
                                |> getFirstRoom (mouseMoveGlobalCoords |> isOnRoom)
                                |> Maybe.map .id
                    in
                    case state of
                        NotHoveringOverRoom ->
                            case roomImHoveringOver of
                                Just room ->
                                    ( { model | mode = Select { selected = selected, state = HoveringOverRoom room } }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        ------------------------------------------
                        HoldingClickOutsideAnyRooms ->
                            ( { model
                                | mode =
                                    Select
                                        { selected = selected
                                        , state =
                                            DraggingToSelectMany
                                                { origin = mouseMoveRelCoords
                                                , end = mouseMoveRelCoords
                                                }
                                        }
                              }
                            , Cmd.none
                            )

                        DraggingToSelectMany { origin } ->
                            ( { model
                                | mode =
                                    Select
                                        { selected = selected
                                        , state =
                                            DraggingToSelectMany
                                                { origin = origin
                                                , end = mouseMoveRelCoords
                                                }
                                        }
                              }
                            , Cmd.none
                            )

                        DraggingRooms { rooms, dragOrigin, dragEnd } ->
                            let
                                excludingCurrentlySelectedRooms initialRooms =
                                    List.filter (\r -> not <| List.any (\e -> e == r.id) rooms) initialRooms
                            in
                            ( { model
                                | mode =
                                    Select
                                        { selected = selected
                                        , state =
                                            DraggingRooms
                                                { rooms = rooms
                                                , dragOrigin = dragOrigin
                                                , dragEnd = mouseMoveRelCoords |> toGlobal model.viewport
                                                , isOverlappingAnotherRoom =
                                                    model.rooms
                                                        |> List.any
                                                            (\r ->
                                                                List.any
                                                                    (\e ->
                                                                        if r.id == e then
                                                                            let
                                                                                ( mInitX, mInitY ) =
                                                                                    dragOrigin

                                                                                ( mX, mY ) =
                                                                                    dragEnd

                                                                                ( x1, y1 ) =
                                                                                    r.boundingBox |> Rect.topLeft

                                                                                ( newX1, newY1 ) =
                                                                                    ( mX - (mInitX - x1)
                                                                                    , mY - (mInitY - y1)
                                                                                    )

                                                                                isOverlappingAnotherRoom : Bool
                                                                                isOverlappingAnotherRoom =
                                                                                    model.rooms
                                                                                        |> excludingCurrentlySelectedRooms
                                                                                        |> List.filter
                                                                                            (.boundingBox
                                                                                                >> Rect.isThereAnyOverlap
                                                                                                    { x1 = newX1
                                                                                                    , y1 = newY1
                                                                                                    , width = r.boundingBox.width
                                                                                                    , height = r.boundingBox.height
                                                                                                    }
                                                                                            )
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
                              }
                            , Cmd.none
                            )

                        ------------------------------------------
                        HoveringOverRoom _ ->
                            case roomImHoveringOver of
                                Just room ->
                                    ( { model | mode = Select { selected = selected, state = HoveringOverRoom room } }, Cmd.none )

                                Nothing ->
                                    ( { model | mode = Select { selected = selected, state = NotHoveringOverRoom } }, Cmd.none )

                        HoldingClickOnRoom room ->
                            let
                                draggingSingleRoom =
                                    ( { model
                                        | mode =
                                            Select
                                                { selected = selected
                                                , state =
                                                    DraggingRoom
                                                        { room = room
                                                        , dragOrigin = mouseMoveRelCoords |> toGlobal model.viewport
                                                        , dragEnd = mouseMoveRelCoords |> toGlobal model.viewport
                                                        , isOverlappingAnotherRoom = False
                                                        , sidesSnapping = { bottom = Nothing }
                                                        }
                                                }
                                      }
                                    , Cmd.none
                                    )
                            in
                            case selected of
                                GroupSelected rooms ->
                                    if List.member room rooms then
                                        ( { model
                                            | mode =
                                                Select
                                                    { selected = selected
                                                    , state =
                                                        DraggingRooms
                                                            { rooms = rooms
                                                            , dragEnd = mouseMoveRelCoords |> toGlobal model.viewport
                                                            , dragOrigin = mouseMoveRelCoords |> toGlobal model.viewport
                                                            , isOverlappingAnotherRoom = False
                                                            }
                                                    }
                                          }
                                        , Cmd.none
                                        )

                                    else
                                        draggingSingleRoom

                                RoomSelected _ ->
                                    draggingSingleRoom

                                NoRoomSelected ->
                                    draggingSingleRoom

                        DraggingRoom { room, dragOrigin, dragEnd } ->
                            case
                                model.rooms
                                    |> getFirstRoom (\r -> r.id == room)
                            of
                                Just roomImDraggingAround ->
                                    let
                                        deltaDrag : Point
                                        deltaDrag =
                                            Point.subtract dragEnd dragOrigin

                                        draggingRectPos : Point
                                        draggingRectPos =
                                            roomImDraggingAround.boundingBox |> Rect.topLeft

                                        draggingRoomCurrentPosition : Rectangle
                                        draggingRoomCurrentPosition =
                                            Point.add draggingRectPos deltaDrag
                                                |> Rect.changePosition roomImDraggingAround.boundingBox

                                        bottomSideSnappingToAnotherRectangle : Maybe Room
                                        bottomSideSnappingToAnotherRectangle =
                                            model.rooms
                                                |> List.filter (\r -> r.id /= room)
                                                |> List.filter
                                                    (\rect ->
                                                        numWithinRange
                                                            (Rect.bottomY draggingRoomCurrentPosition)
                                                            (Rect.bottomY rect.boundingBox)
                                                            snapDistanceRange
                                                    )
                                                |> closestRoomX { roomImDraggingAround | boundingBox = draggingRoomCurrentPosition }
                                                |> Maybe.andThen
                                                    (\roomToSnapTo ->
                                                        if
                                                            numWithinRange
                                                                (Rect.center draggingRoomCurrentPosition |> Point.x)
                                                                (Rect.center roomToSnapTo.boundingBox |> Point.x)
                                                                snapMinValidDistance
                                                        then
                                                            Just roomToSnapTo

                                                        else
                                                            Nothing
                                                    )
                                    in
                                    ( { model
                                        | mode =
                                            Select
                                                { selected = selected
                                                , state =
                                                    DraggingRoom
                                                        { room = room
                                                        , dragOrigin = dragOrigin
                                                        , dragEnd = mouseMoveRelCoords |> toGlobal model.viewport
                                                        , sidesSnapping = { bottom = bottomSideSnappingToAnotherRectangle }
                                                        , isOverlappingAnotherRoom =
                                                            model.rooms
                                                                |> List.any
                                                                    (\r ->
                                                                        if r.id == room then
                                                                            let
                                                                                rectPos : Point
                                                                                rectPos =
                                                                                    r.boundingBox |> Rect.topLeft

                                                                                ( newX, newY ) =
                                                                                    Point.add rectPos deltaDrag

                                                                                newRectangle : Rectangle
                                                                                newRectangle =
                                                                                    { x1 = newX
                                                                                    , y1 = newY
                                                                                    , width = r.boundingBox.width
                                                                                    , height = r.boundingBox.height
                                                                                    }

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
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

        MouseUp mouseUpRelCoords ->
            case model.mode of
                Pan state ->
                    case state of
                        NotPanning ->
                            ( model, Cmd.none )

                        Panning { panOrigin, panEnd } ->
                            let
                                panDist =
                                    Point.subtract panOrigin panEnd
                            in
                            ( { model
                                | viewport = model.viewport |> Point.add panDist
                                , mode = Pan NotPanning
                              }
                            , Cmd.none
                            )

                Select { selected, state } ->
                    case state of
                        NotHoveringOverRoom ->
                            ( model, Cmd.none )

                        HoldingClickOutsideAnyRooms ->
                            ( { model
                                | mode =
                                    Select
                                        { selected = NoRoomSelected
                                        , state = NotHoveringOverRoom
                                        }
                              }
                            , Cmd.none
                            )

                        DraggingToSelectMany { origin, end } ->
                            let
                                selectArea : Rectangle
                                selectArea =
                                    pointsToRectangle (origin |> toGlobal model.viewport) (end |> toGlobal model.viewport)

                                roomsSelected : List Room
                                roomsSelected =
                                    model.rooms
                                        |> List.filter (.boundingBox >> Rect.isInside selectArea)
                            in
                            ( { model
                                | mode =
                                    Select
                                        { selected =
                                            if List.isEmpty roomsSelected then
                                                selected

                                            else
                                                case roomsSelected of
                                                    [ room ] ->
                                                        RoomSelected room.id

                                                    _ ->
                                                        GroupSelected (roomsSelected |> List.map .id)
                                        , state = NotHoveringOverRoom
                                        }
                              }
                            , Cmd.none
                            )

                        HoveringOverRoom _ ->
                            ( model, Cmd.none )

                        HoldingClickOnRoom roomId ->
                            ( { model
                                | mode =
                                    Select
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
                              }
                            , Cmd.none
                            )

                        DraggingRoom { room, dragOrigin, dragEnd, isOverlappingAnotherRoom, sidesSnapping } ->
                            ( { model
                                | rooms =
                                    if isOverlappingAnotherRoom then
                                        model.rooms

                                    else
                                        model.rooms
                                            |> List.map
                                                (\r ->
                                                    if r.id == room then
                                                        let
                                                            deltaDrag =
                                                                Point.subtract dragEnd dragOrigin

                                                            rectPos : Point
                                                            rectPos =
                                                                r.boundingBox |> Rect.topLeft

                                                            ( newX, newY ) =
                                                                Point.add rectPos deltaDrag

                                                            newRectangle : Rectangle
                                                            newRectangle =
                                                                { x1 = newX
                                                                , y1 =
                                                                    case sidesSnapping.bottom of
                                                                        Just snappingBottomRoom ->
                                                                            -- same bottom y as the room we are snapping to
                                                                            (snappingBottomRoom.boundingBox.y1 + snappingBottomRoom.boundingBox.height) - r.boundingBox.height

                                                                        Nothing ->
                                                                            newY
                                                                , width = r.boundingBox.width
                                                                , height = r.boundingBox.height
                                                                }
                                                        in
                                                        { r | boundingBox = newRectangle }

                                                    else
                                                        r
                                                )
                                , mode = Select { selected = selected, state = HoveringOverRoom room }
                              }
                            , Cmd.none
                            )

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
                                                        rectPos : Point
                                                        rectPos =
                                                            r.boundingBox |> Rect.topLeft

                                                        ( newX, newY ) =
                                                            Point.add rectPos deltaDrag

                                                        newRectangle : Rectangle
                                                        newRectangle =
                                                            { x1 = newX
                                                            , y1 = newY
                                                            , width = r.boundingBox.width
                                                            , height = r.boundingBox.height
                                                            }

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
                            ( { model
                                | rooms =
                                    if anySelectedRoomIsOverlappingARoom then
                                        model.rooms

                                    else
                                        model.rooms
                                            |> List.map
                                                (\r ->
                                                    if List.any (\e -> r.id == e) rooms then
                                                        let
                                                            rectPos : Point
                                                            rectPos =
                                                                r.boundingBox |> Rect.topLeft

                                                            ( newX, newY ) =
                                                                Point.add rectPos deltaDrag

                                                            newRectangle : Rectangle
                                                            newRectangle =
                                                                { x1 = newX
                                                                , y1 = newY
                                                                , width = r.boundingBox.width
                                                                , height = r.boundingBox.height
                                                                }
                                                        in
                                                        { r | boundingBox = newRectangle }

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
                            , Cmd.none
                            )

                Delete ->
                    let
                        globalMouseUpCoords =
                            mouseUpRelCoords |> toGlobal model.viewport
                    in
                    ( { model
                        | rooms =
                            model.rooms |> List.filter (globalMouseUpCoords |> isNotOnRoom)
                      }
                    , Cmd.none
                    )

                Draw _ ->
                    ( model, Cmd.none )


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

                        Pan _ ->
                            let
                                isPanning =
                                    case model.mode of
                                        Pan _ ->
                                            True

                                        _ ->
                                            False
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
                            [ on "click" (mouseMoveDecoder |> JD.map MouseDown)
                            , case state of
                                NotDrawing ->
                                    style "" ""

                                SelectedStart _ ->
                                    on "mousemove" (mouseMoveDecoder |> JD.map MouseMove)
                            ]

                        Select _ ->
                            [ on "mousemove" (mouseMoveDecoder |> JD.map MouseMove)
                            , on "mousedown" (mouseMoveDecoder |> JD.map MouseDown)
                            , on "mouseup" (mouseMoveDecoder |> JD.map MouseUp)
                            ]
                   )
            )
            -- Drawing order is bottom to top, draw last on top
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

                    drawSnapLines : List (Svg Msg)
                    drawSnapLines =
                        case model.snappingPointsLine of
                            Just ( first, second ) ->
                                drawSnappingLines model.viewport first second

                            Nothing ->
                                []
                 in
                 -- NOTE: Drawing order is top to bottom, draw on top last
                 (case model.mode of
                    Delete ->
                        bgGrid ++ drawRooms model.rooms

                    Pan _ ->
                        bgGrid ++ drawRooms model.rooms

                    Draw state ->
                        bgGrid
                            ++ (case state of
                                    NotDrawing ->
                                        drawRooms model.rooms

                                    SelectedStart { start, end, isOverlappingAnotherRoom } ->
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
                                    DraggingRoom { room, dragOrigin, dragEnd, isOverlappingAnotherRoom, sidesSnapping } ->
                                        rooms
                                            |> getFirstRoom (\{ id } -> id == room)
                                            |> Maybe.map
                                                (\{ boundingBox } ->
                                                    let
                                                        { width, height, x1, y1 } =
                                                            boundingBox

                                                        ( initialMx1, initialMy1 ) =
                                                            dragOrigin

                                                        ( mx1, my1 ) =
                                                            dragEnd

                                                        ( gx, gy ) =
                                                            model.viewport

                                                        distFromSelectedX =
                                                            initialMx1 - mx1

                                                        distFromSelectedY =
                                                            initialMy1 - my1
                                                    in
                                                    drawRect
                                                        { x1 = (x1 - distFromSelectedX) - gx
                                                        , y1 =
                                                            case sidesSnapping.bottom of
                                                                Just snappingBottomRoom ->
                                                                    -- same bottom y as the room we are snapping to
                                                                    (snappingBottomRoom.boundingBox.y1 + snappingBottomRoom.boundingBox.height) - boundingBox.height

                                                                Nothing ->
                                                                    (y1 - distFromSelectedY) - gy
                                                        , height = height
                                                        , width = width
                                                        }
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
                                                        { width, height, x1, y1 } =
                                                            boundingBox

                                                        ( initialMx1, initialMy1 ) =
                                                            dragOrigin

                                                        ( mx1, my1 ) =
                                                            dragEnd

                                                        ( gx, gy ) =
                                                            model.viewport

                                                        distFromSelectedX =
                                                            initialMx1 - mx1

                                                        distFromSelectedY =
                                                            initialMy1 - my1
                                                    in
                                                    drawRect
                                                        { x1 = (x1 - distFromSelectedX) - gx
                                                        , y1 = (y1 - distFromSelectedY) - gy
                                                        , height = height
                                                        , width = width
                                                        }
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
                 )
                    ++ drawSnapLines
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


getSnappingRooms :
    Room
    -> List Room
    ->
        ( Maybe
            { currentRect : RoomPossibleSnappingX
            , room : Room
            , snappingOn : RoomPossibleSnappingX
            }
        , Maybe
            { currentRect : RoomPossibleSnappingY
            , room : Room
            , snappingOn : RoomPossibleSnappingY
            }
        )
getSnappingRooms room allRooms =
    let
        horizontal :
            Maybe
                { currentRect : RoomPossibleSnappingX
                , room : Room
                , snappingOn : RoomPossibleSnappingX
                }
        horizontal =
            allRooms
                |> List.filter (\r -> r.id /= room.id)
                |> List.filter
                    (\r ->
                        -- if rectangle y is withing range for snapping
                        let
                            y1 =
                                room.boundingBox.y1

                            y2 =
                                room.boundingBox.y1 + room.boundingBox.height

                            y3 =
                                r.boundingBox.y1 - snapDistanceRange

                            y4 =
                                r.boundingBox.y1 + r.boundingBox.height + snapDistanceRange
                        in
                        -- is inside
                        (y3 <= y1 && y2 <= y4)
                            -- any bottom portion is inside
                            || (y4 <= y2 && y4 >= y1)
                            -- any top portion is inside
                            || (y3 >= y1 && y3 <= y2)
                    )
                |> closestRoomX room
                |> Maybe.andThen
                    (\currRoom ->
                        let
                            topY =
                                currRoom.boundingBox.y1

                            centerY =
                                currRoom.boundingBox |> Rect.center |> Point.y

                            bottomY =
                                currRoom.boundingBox |> Rect.bottomY

                            bottomYWithinRangeOf number =
                                numWithinRange (room.boundingBox |> Rect.bottomY) number snapDistanceRange

                            middleYWithinRangeOf number =
                                numWithinRange (room.boundingBox |> Rect.center |> Point.y) number snapDistanceRange

                            topYWithinRangeOf number =
                                numWithinRange room.boundingBox.y1 number snapDistanceRange
                        in
                        -- NOTE: some of these could be skipped depending on the position of currRoom, refactor later?
                        if bottomYWithinRangeOf topY then
                            Just
                                { currentRect = SnappingXBottom
                                , room = currRoom
                                , snappingOn = SnappingXTop
                                }

                        else if bottomYWithinRangeOf centerY then
                            Just
                                { currentRect = SnappingXBottom
                                , snappingOn = SnappingXMiddle
                                , room = currRoom
                                }

                        else if bottomYWithinRangeOf bottomY then
                            Just
                                { currentRect = SnappingXBottom
                                , snappingOn = SnappingXBottom
                                , room = currRoom
                                }

                        else if middleYWithinRangeOf topY then
                            Just
                                { currentRect = SnappingXMiddle
                                , snappingOn = SnappingXTop
                                , room = currRoom
                                }

                        else if middleYWithinRangeOf centerY then
                            Just
                                { currentRect = SnappingXMiddle
                                , snappingOn = SnappingXMiddle
                                , room = currRoom
                                }

                        else if middleYWithinRangeOf bottomY then
                            Just
                                { currentRect = SnappingXMiddle
                                , snappingOn = SnappingXBottom
                                , room = currRoom
                                }

                        else if topYWithinRangeOf topY then
                            Just
                                { currentRect = SnappingXTop
                                , snappingOn = SnappingXTop
                                , room = currRoom
                                }

                        else if topYWithinRangeOf centerY then
                            Just
                                { currentRect = SnappingXTop
                                , snappingOn = SnappingXMiddle
                                , room = currRoom
                                }

                        else if topYWithinRangeOf bottomY then
                            Just
                                { currentRect = SnappingXTop
                                , snappingOn = SnappingXBottom
                                , room = currRoom
                                }

                        else
                            Nothing
                    )

        vertical :
            Maybe
                { currentRect : RoomPossibleSnappingY
                , snappingOn : RoomPossibleSnappingY
                , room : Room
                }
        vertical =
            allRooms
                |> List.filter (\r -> r.id /= room.id)
                |> List.filter
                    (\r ->
                        -- if rectangle x is withing range for snapping
                        let
                            x1 =
                                room.boundingBox.x1

                            x2 =
                                room.boundingBox.x1 + room.boundingBox.width

                            x3 =
                                r.boundingBox.x1 - snapDistanceRange

                            x4 =
                                r.boundingBox.x1 + r.boundingBox.width + snapDistanceRange
                        in
                        -- is inside
                        (x3 <= x1 && x2 <= x4)
                            -- any right portion is inside
                            || (x4 <= x2 && x4 >= x1)
                            -- any left portion is inside
                            || (x3 >= x1 && x3 <= x2)
                    )
                |> closestRoomY room
                |> Maybe.andThen
                    (\currRoom ->
                        let
                            leftX =
                                currRoom.boundingBox.x1

                            centerX =
                                currRoom.boundingBox |> Rect.center |> Point.x

                            rightX =
                                currRoom.boundingBox |> Rect.rightX

                            rightXWithinRangeOf number =
                                numWithinRange (room.boundingBox |> Rect.rightX) number snapDistanceRange

                            centerXWithinRangeOf number =
                                numWithinRange (room.boundingBox |> Rect.center |> Point.x) number snapDistanceRange

                            leftXWithinRangeOf number =
                                numWithinRange room.boundingBox.x1 number snapDistanceRange
                        in
                        if rightXWithinRangeOf leftX then
                            Just
                                { currentRect = SnappingYRight
                                , snappingOn = SnappingYLeft
                                , room = currRoom
                                }

                        else if rightXWithinRangeOf centerX then
                            Just
                                { currentRect = SnappingYRight
                                , snappingOn = SnappingYMiddle
                                , room = currRoom
                                }

                        else if rightXWithinRangeOf rightX then
                            Just
                                { currentRect = SnappingYRight
                                , snappingOn = SnappingYRight
                                , room = currRoom
                                }

                        else if centerXWithinRangeOf leftX then
                            Just
                                { currentRect = SnappingYMiddle
                                , snappingOn = SnappingYLeft
                                , room = currRoom
                                }

                        else if centerXWithinRangeOf centerX then
                            Just
                                { currentRect = SnappingYMiddle
                                , snappingOn = SnappingYMiddle
                                , room = currRoom
                                }

                        else if centerXWithinRangeOf rightX then
                            Just
                                { currentRect = SnappingYMiddle
                                , snappingOn = SnappingYRight
                                , room = currRoom
                                }

                        else if leftXWithinRangeOf leftX then
                            Just
                                { currentRect = SnappingYLeft
                                , snappingOn = SnappingYLeft
                                , room = currRoom
                                }

                        else if leftXWithinRangeOf centerX then
                            Just
                                { currentRect = SnappingYLeft
                                , snappingOn = SnappingYMiddle
                                , room = currRoom
                                }

                        else if leftXWithinRangeOf rightX then
                            Just
                                { currentRect = SnappingYLeft
                                , snappingOn = SnappingYRight
                                , room = currRoom
                                }

                        else
                            Nothing
                    )
    in
    ( horizontal, vertical )
