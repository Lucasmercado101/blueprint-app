port module Main exposing (..)

import Browser
import Browser.Events exposing (onMouseMove)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events exposing (on, onClick, onInput, onMouseDown, onMouseUp)
import Json.Decode as JD exposing (Decoder)
import Point
import Random
import Rect exposing (Point, Rectangle)
import Svg as S exposing (Svg, line, rect, svg)
import Svg.Attributes as SA exposing (color, cx, cy, fill, fontSize, r, rx, ry, stroke, strokeWidth, version, viewBox, x1, x2, y1, y2)
import UUID exposing (UUID)



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
    receiveGotSvgBoundingBox ReceivedSvgBoundingBox


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
    , name :
        { id : RoomID
        , value : String
        , boundingBox : Maybe Rectangle
        }
    }


type alias Model =
    { mapPanOffset : Point

    -- TODO: move this to Drag
    , relativeView :
        { start : Point
        , originalView : Point
        }
    , mode : Mode
    , holdingLeftMouseDown : Bool
    , rooms :
        List Room
    , snappingPointsLine : Maybe ( Line, Line )
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mapPanOffset = ( 0, 0 )
      , relativeView =
            { start = ( 0, 0 )
            , originalView = ( 0, 0 )
            }
      , mode = Pan
      , holdingLeftMouseDown = False
      , rooms = []
      , snappingPointsLine = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Mode
    = Pan
    | Draw DrawState
    | Select
        { selected : SelectedRoom
        , state : HoveringOverOrDraggingRoom
        }
    | Delete


type SelectedRoom
    = NoRoomSelected
    | RoomSelected RoomID
    | GroupSelected (List RoomID)


type HoveringOverOrDraggingRoom
    = NotHoveringOverRoom
    | HoldingClickOutsideAnyRooms
    | DraggingToSelectMany
        { start : Point
        , end : Point
        }
    | HoveringOverRoom RoomID
    | HoldingClickOnRoom RoomID
    | DraggingRoom
        { room : RoomID
        , mousePos : Point
        , initialMousePos : Point
        , isOverlappingAnotherRoom : Bool
        }


type DrawState
    = NotDrawing
      -- NOTE: "relativeStartingPoint" is there so that
      -- the rectangle's start is always smaller than its end at the end of the day
    | SelectedStart
        { position : { start : Point, end : Point }
        , relativeStartingPoint : Point
        , isOverlappingAnotherRoom : Bool
        }


type Msg
    = NoOp
    | MouseMove Point
    | MouseDown Point
    | MouseUp Point
    | DrawMode
    | PanMode
    | DeleteMode
    | SelectMode
    | OnChangeRectangleName ( RoomID, String )
    | ReceivedSvgBoundingBox JD.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DeleteMode ->
            ( { model
                | mode = Delete
              }
            , Cmd.none
            )

        ReceivedSvgBoundingBox value ->
            case JD.decodeValue receiveGotSvgBoundingBoxDecoder value of
                Ok ( idStr, boundingBox ) ->
                    case UUID.fromString idStr of
                        Ok textId ->
                            ( { model
                                | rooms =
                                    model.rooms
                                        |> List.map
                                            (\rect ->
                                                if rect.name.id == textId then
                                                    let
                                                        name =
                                                            rect.name
                                                    in
                                                    { rect
                                                        | name =
                                                            { name
                                                                | boundingBox = Just boundingBox
                                                            }
                                                    }

                                                else
                                                    rect
                                            )
                              }
                            , Cmd.none
                            )

                        Err err ->
                            ( model, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        OnChangeRectangleName ( rectId, newName ) ->
            let
                textId =
                    List.filter (\rect -> rect.id == rectId) model.rooms
                        |> List.head
                        |> Maybe.map (.name >> .id)
            in
            ( { model
                | rooms =
                    model.rooms
                        |> List.map
                            (\rect ->
                                if rect.id == rectId then
                                    let
                                        name =
                                            rect.name
                                    in
                                    { rect
                                        | name =
                                            { name
                                                | value = newName
                                            }
                                    }

                                else
                                    rect
                            )
              }
            , case textId of
                Just id ->
                    requestGetSvgBoundingBox (id |> UUID.toString)

                Nothing ->
                    Cmd.none
            )

        DrawMode ->
            ( { model
                | mode = Draw NotDrawing
                , relativeView =
                    { start = ( 0, 0 )
                    , originalView = ( 0, 0 )
                    }
              }
            , Cmd.none
            )

        PanMode ->
            ( { model
                | mode = Pan
                , relativeView =
                    { start = ( 0, 0 )
                    , originalView = ( 0, 0 )
                    }
              }
            , Cmd.none
            )

        SelectMode ->
            ( { model
                | mode =
                    Select
                        { selected = NoRoomSelected
                        , state = NotHoveringOverRoom
                        }
                , relativeView =
                    { start = ( 0, 0 )
                    , originalView = ( 0, 0 )
                    }
              }
            , Cmd.none
            )

        MouseDown (( x, y ) as mouseDownRelCoords) ->
            case model.mode of
                Delete ->
                    ( { model
                        | rooms =
                            model.rooms
                                |> List.filter ((\{ boundingBox } -> Rect.isPointOnRectangle (mouseDownRelCoords |> toGlobal model.mapPanOffset) boundingBox) >> not)
                      }
                    , Cmd.none
                    )

                Pan ->
                    ( { model
                        | holdingLeftMouseDown = True
                        , relativeView =
                            { start = ( x, y )
                            , originalView = model.mapPanOffset
                            }
                      }
                    , Cmd.none
                    )

                Draw state ->
                    case state of
                        NotDrawing ->
                            ( { model
                                | mode =
                                    Draw
                                        (SelectedStart
                                            { position = { start = ( x, y ), end = ( x, y ) }
                                            , isOverlappingAnotherRoom = False
                                            , relativeStartingPoint = ( x, y )
                                            }
                                        )
                                , snappingPointsLine = Nothing
                              }
                            , Cmd.none
                            )

                        SelectedStart { position, isOverlappingAnotherRoom } ->
                            if isOverlappingAnotherRoom then
                                ( { model
                                    | mode = Draw NotDrawing
                                    , snappingPointsLine = Nothing
                                  }
                                , Cmd.none
                                )

                            else
                                let
                                    { start, end } =
                                        position

                                    ( x1, y1 ) =
                                        start

                                    ( x2, y2 ) =
                                        end

                                    ( ox, oy ) =
                                        model.mapPanOffset

                                    nameId =
                                        Random.step UUID.generator (Random.initialSeed (x1 + y1 + x2 + y2 + ox + oy + 54321))
                                            |> Tuple.first
                                in
                                ( { model
                                    | rooms =
                                        { boundingBox =
                                            { x1 = x1 + ox
                                            , y1 = y1 + oy
                                            , width = x2 - x1
                                            , height = y2 - y1
                                            }

                                        --   TODO: change to use random, have it be a command
                                        , id =
                                            Random.step UUID.generator (Random.initialSeed (x1 + y1 + x2 + y2 + ox + oy + 12345))
                                                |> Tuple.first
                                        , name =
                                            { id = nameId
                                            , value = ""
                                            , boundingBox = Nothing
                                            }
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
                        onARoom : Maybe RoomID
                        onARoom =
                            model.rooms
                                |> getFirstRoom (\{ boundingBox } -> Rect.isPointOnRectangle (mouseDownRelCoords |> toGlobal model.mapPanOffset) boundingBox)
                                |> Maybe.map .id
                    in
                    case onARoom of
                        Just room ->
                            ( { model | mode = Select { selected = selected, state = HoldingClickOnRoom room } }, Cmd.none )

                        Nothing ->
                            ( { model | mode = Select { selected = selected, state = HoldingClickOutsideAnyRooms } }, Cmd.none )

        MouseMove (( x, y ) as mouseMoveRelCoords) ->
            case model.mode of
                Delete ->
                    ( model, Cmd.none )

                Pan ->
                    let
                        ( sx, sy ) =
                            model.relativeView.start

                        ( ox, oy ) =
                            model.relativeView.originalView

                        ( dx, dy ) =
                            ( sx - x, sy - y )

                        maxTopPan : Maybe Rectangle
                        maxTopPan =
                            List.map .boundingBox model.rooms |> Rect.topmostRectangle

                        maxBottomPan : Maybe Rectangle
                        maxBottomPan =
                            List.map .boundingBox model.rooms |> Rect.bottommostRectangle

                        maxLeftPan : Maybe Rectangle
                        maxLeftPan =
                            List.map .boundingBox model.rooms |> Rect.leftmostRectangle

                        maxRightPan : Maybe Rectangle
                        maxRightPan =
                            List.map .boundingBox model.rooms |> Rect.rightmostRectangle

                        -- NOTE:
                        -- these are in charge of clamping the pan
                        -- just use the 'else' case if i want to undo it
                        newY =
                            case ( maxTopPan, maxBottomPan ) of
                                ( Just maxY, Just minY ) ->
                                    clamp (maxY.y1 - screenHeight - 50) (minY.y1 + minY.height + 50) (oy + dy)

                                _ ->
                                    oy + dy

                        newX =
                            case ( maxLeftPan, maxRightPan ) of
                                ( Just maxX, Just minX ) ->
                                    clamp (maxX.x1 - screenWidth - 50) (minX.x1 + minX.width + 50) (ox + dx)

                                _ ->
                                    ox + dx
                    in
                    ( { model
                        | mapPanOffset = ( newX, newY )
                      }
                    , Cmd.none
                    )

                Draw state ->
                    case state of
                        NotDrawing ->
                            ( model, Cmd.none )

                        SelectedStart ({ relativeStartingPoint } as selectedStart) ->
                            let
                                ( xStart, yStart ) =
                                    relativeStartingPoint

                                position =
                                    if x <= xStart && y <= yStart then
                                        { start = ( x, y ), end = relativeStartingPoint }

                                    else if y <= yStart then
                                        { start = ( xStart, y ), end = ( x, yStart ) }

                                    else if x <= xStart then
                                        { start = ( x, yStart ), end = ( xStart, y ) }

                                    else
                                        { start = relativeStartingPoint, end = ( x, y ) }

                                isOverlappingAnotherRoom : Bool
                                isOverlappingAnotherRoom =
                                    let
                                        ( gx, gy ) =
                                            toGlobal position.start model.mapPanOffset

                                        ( w1, h1 ) =
                                            toGlobal position.end model.mapPanOffset |> (\( x1, y1 ) -> ( x1 - gx, y1 - gy ))
                                    in
                                    List.any
                                        (\rect ->
                                            Rect.isThereAnyOverlap
                                                { x1 = gx
                                                , y1 = gy
                                                , width = w1
                                                , height = h1
                                                }
                                                rect
                                        )
                                        (List.map .boundingBox model.rooms)

                                bottomSideIsAlignedToAnotherRectangle : Maybe ( Line, Line )
                                bottomSideIsAlignedToAnotherRectangle =
                                    let
                                        -- top left
                                        ( tlx, tly ) =
                                            toGlobal position.start model.mapPanOffset

                                        -- bottom right
                                        ( brx, bry ) =
                                            toGlobal position.end model.mapPanOffset

                                        currDrawRect : Rectangle
                                        currDrawRect =
                                            { x1 = tlx
                                            , y1 = tly
                                            , width = brx - tlx
                                            , height = bry - tly
                                            }
                                    in
                                    model.rooms
                                        |> List.map .boundingBox
                                        |> List.filter (\rect -> numWithinRange (rect |> Rect.bottomY) bry 10)
                                        |> (\l ->
                                                let
                                                    closestRectangleToTheRight : Maybe Rectangle
                                                    closestRectangleToTheRight =
                                                        List.foldl
                                                            (\next curr ->
                                                                let
                                                                    x1 =
                                                                        next |> Rect.bottomLeft |> Point.x
                                                                in
                                                                case curr of
                                                                    Just currBiggest ->
                                                                        let
                                                                            x2 =
                                                                                currBiggest |> Rect.bottomLeft |> Point.x
                                                                        in
                                                                        if brx <= x1 && x1 <= x2 then
                                                                            Just next

                                                                        else
                                                                            curr

                                                                    Nothing ->
                                                                        if brx <= x1 then
                                                                            Just next

                                                                        else
                                                                            Nothing
                                                            )
                                                            Nothing
                                                            l

                                                    closestRectangleToTheLeft : Maybe Rectangle
                                                    closestRectangleToTheLeft =
                                                        List.foldl
                                                            (\next curr ->
                                                                let
                                                                    ( x1, _ ) =
                                                                        Rect.bottomLeft next
                                                                in
                                                                case curr of
                                                                    Just val ->
                                                                        if x1 <= tlx && x1 >= (Rect.bottomLeft val |> Tuple.first) then
                                                                            Just next

                                                                        else
                                                                            curr

                                                                    Nothing ->
                                                                        if x1 <= tlx then
                                                                            Just next

                                                                        else
                                                                            Nothing
                                                            )
                                                            Nothing
                                                            l

                                                    closestRectangle : Maybe Rectangle
                                                    closestRectangle =
                                                        case ( closestRectangleToTheRight, closestRectangleToTheLeft ) of
                                                            ( Just rr, Just rl ) ->
                                                                let
                                                                    x1 =
                                                                        rr |> Rect.bottomLeft |> Point.x

                                                                    x2 =
                                                                        rl |> Rect.bottomRight |> Point.x
                                                                in
                                                                if (abs tlx - abs x2) > (abs x1 - abs brx) then
                                                                    Just rr

                                                                else
                                                                    Just rl

                                                            ( Just r1, Nothing ) ->
                                                                Just r1

                                                            ( Nothing, Just r2 ) ->
                                                                Just r2

                                                            ( Nothing, Nothing ) ->
                                                                Nothing
                                                in
                                                closestRectangle
                                           )
                                        |> Maybe.andThen
                                            (\rect ->
                                                let
                                                    bl =
                                                        rect |> Rect.bottomLeft |> Point.x

                                                    br =
                                                        rect |> Rect.bottomRight |> Point.x

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
                                                        150
                                                in
                                                if isOnTheRight then
                                                    if rightDist <= minDistBeforeSnapping then
                                                        Just
                                                            ( ( currDrawRect |> Rect.bottomLeft |> Tuple.mapSecond (always (rect |> Rect.bottomY))
                                                              , currDrawRect |> Rect.bottomRight |> Tuple.mapSecond (always (rect |> Rect.bottomY))
                                                              )
                                                            , rect |> Rect.bottomSide
                                                            )

                                                    else
                                                        Nothing

                                                else if leftDist <= minDistBeforeSnapping then
                                                    Just
                                                        ( rect |> Rect.bottomSide
                                                        , ( currDrawRect |> Rect.bottomLeft |> Tuple.mapSecond (always (rect |> Rect.bottomY))
                                                          , currDrawRect |> Rect.bottomRight |> Tuple.mapSecond (always (rect |> Rect.bottomY))
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
                                                toRelative l model.mapPanOffset |> Point.y
                                            )
                                        |> Maybe.map
                                            (\e ->
                                                { start = position.start
                                                , end = position.end |> Tuple.mapSecond (always e)
                                                }
                                            )
                            in
                            ( { model
                                | mode =
                                    Draw
                                        (SelectedStart
                                            { selectedStart
                                                | position = snapBottomPosition |> Maybe.withDefault position
                                                , isOverlappingAnotherRoom = isOverlappingAnotherRoom
                                            }
                                        )
                                , snappingPointsLine = bottomSideIsAlignedToAnotherRectangle
                              }
                            , Cmd.none
                            )

                Select { selected, state } ->
                    let
                        roomImHoveringOver : Maybe RoomID
                        roomImHoveringOver =
                            model.rooms
                                |> getFirstRoom (\{ boundingBox } -> Rect.isPointOnRectangle (mouseMoveRelCoords |> toGlobal model.mapPanOffset) boundingBox)
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
                                                { start = mouseMoveRelCoords |> toGlobal model.mapPanOffset
                                                , end = mouseMoveRelCoords |> toGlobal model.mapPanOffset
                                                }
                                        }
                              }
                            , Cmd.none
                            )

                        DraggingToSelectMany { start } ->
                            ( { model
                                | mode =
                                    Select
                                        { selected = selected
                                        , state =
                                            DraggingToSelectMany
                                                { start = start
                                                , end = mouseMoveRelCoords |> toGlobal model.mapPanOffset
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
                            ( { model
                                | mode =
                                    Select
                                        { selected = selected
                                        , state =
                                            DraggingRoom
                                                { room = room
                                                , mousePos = mouseMoveRelCoords |> toGlobal model.mapPanOffset
                                                , initialMousePos = mouseMoveRelCoords |> toGlobal model.mapPanOffset
                                                , isOverlappingAnotherRoom = False
                                                }
                                        }
                              }
                            , Cmd.none
                            )

                        DraggingRoom { room, initialMousePos, mousePos } ->
                            ( { model
                                | mode =
                                    Select
                                        { selected = selected
                                        , state =
                                            DraggingRoom
                                                { room = room
                                                , initialMousePos = initialMousePos
                                                , mousePos = mouseMoveRelCoords |> toGlobal model.mapPanOffset
                                                , isOverlappingAnotherRoom =
                                                    model.rooms
                                                        |> List.any
                                                            (\r ->
                                                                if r.id == room then
                                                                    let
                                                                        ( mInitX, mInitY ) =
                                                                            initialMousePos

                                                                        ( mX, mY ) =
                                                                            mousePos

                                                                        ( x1, y1 ) =
                                                                            r.boundingBox |> Rect.topLeft

                                                                        ( newX1, newY1 ) =
                                                                            ( mX - (mInitX - x1)
                                                                            , mY - (mInitY - y1)
                                                                            )

                                                                        isOverlappingAnotherRoom : Bool
                                                                        isOverlappingAnotherRoom =
                                                                            model.rooms
                                                                                |> List.filter (\e -> e.id /= room)
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
                                                }
                                        }
                              }
                            , Cmd.none
                            )

        MouseUp _ ->
            case model.mode of
                Pan ->
                    ( { model | holdingLeftMouseDown = False }, Cmd.none )

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

                        DraggingToSelectMany { start, end } ->
                            let
                                roomsSelected : List Room
                                roomsSelected =
                                    model.rooms
                                        |> List.filter
                                            (\r ->
                                                let
                                                    ( x1, y1 ) =
                                                        start

                                                    ( x2, y2 ) =
                                                        end
                                                in
                                                Rect.isInside
                                                    { x1 = x1
                                                    , y1 = y1
                                                    , width = x2 - x1
                                                    , height = y2 - y1
                                                    }
                                                    r.boundingBox
                                            )
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

                        DraggingRoom { room, initialMousePos, mousePos } ->
                            ( { model
                                | rooms =
                                    model.rooms
                                        |> List.map
                                            (\r ->
                                                if r.id == room then
                                                    let
                                                        ( mInitX, mInitY ) =
                                                            initialMousePos

                                                        ( mX, mY ) =
                                                            mousePos

                                                        ( x1, y1 ) =
                                                            r.boundingBox |> Rect.topLeft

                                                        ( newX1, newY1 ) =
                                                            ( mX - (mInitX - x1)
                                                            , mY - (mInitY - y1)
                                                            )

                                                        isOverlappingAnotherRoom : Bool
                                                        isOverlappingAnotherRoom =
                                                            model.rooms
                                                                |> List.filter (\e -> e.id /= room)
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
                                                    if isOverlappingAnotherRoom then
                                                        r

                                                    else
                                                        { r
                                                            | boundingBox =
                                                                { x1 = newX1
                                                                , y1 = newY1
                                                                , width = r.boundingBox.width
                                                                , height = r.boundingBox.height
                                                                }
                                                        }

                                                else
                                                    r
                                            )
                                , mode = Select { selected = selected, state = HoveringOverRoom room }
                              }
                            , Cmd.none
                            )

                Delete ->
                    ( model, Cmd.none )

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
                            [ on "click" (mouseMoveDecoder |> JD.map MouseDown)
                            ]

                        Pan ->
                            [ on "mouseup" (mouseMoveDecoder |> JD.map MouseUp)
                            , if model.holdingLeftMouseDown then
                                on "mousemove" (mouseMoveDecoder |> JD.map MouseMove)

                              else
                                style "" ""
                            , if model.holdingLeftMouseDown then
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
                    bgGrid : List (Svg Msg)
                    bgGrid =
                        backgroundGrid model.mapPanOffset

                    globalRectToRel : Rectangle -> Rectangle
                    globalRectToRel { x1, y1, width, height } =
                        let
                            rel : Point
                            rel =
                                toRelative ( x1, y1 ) model.mapPanOffset
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
                                drawSnappingLines model.mapPanOffset first second

                            Nothing ->
                                []
                 in
                 -- NOTE: Drawing order is top to bottom, draw on top last
                 (case model.mode of
                    Delete ->
                        bgGrid ++ drawRooms model.rooms

                    Pan ->
                        bgGrid ++ drawRooms model.rooms

                    Draw state ->
                        bgGrid
                            ++ (case state of
                                    NotDrawing ->
                                        drawRooms model.rooms

                                    SelectedStart { position, isOverlappingAnotherRoom } ->
                                        let
                                            { start, end } =
                                                position

                                            ( x1, y1 ) =
                                                start

                                            ( x2, y2 ) =
                                                end
                                        in
                                        drawRooms model.rooms
                                            ++ [ drawRect
                                                    { x1 = x1
                                                    , y1 = y1
                                                    , height = y2 - y1
                                                    , width = x2 - x1
                                                    }
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

                            draggingRoom : Maybe RoomID
                            draggingRoom =
                                case state of
                                    DraggingRoom { room } ->
                                        Just room

                                    _ ->
                                        Nothing

                            noDraggingRoom : List Room -> List Room
                            noDraggingRoom initialRooms =
                                case draggingRoom of
                                    Just room ->
                                        initialRooms |> List.filter (\r -> r.id /= room)

                                    Nothing ->
                                        initialRooms

                            drawRoomBeingDragged : List Room -> Svg Msg
                            drawRoomBeingDragged rooms =
                                case state of
                                    DraggingRoom { room, initialMousePos, mousePos, isOverlappingAnotherRoom } ->
                                        rooms
                                            |> List.filter (\{ id } -> id == room)
                                            |> List.head
                                            |> Maybe.map
                                                (\{ boundingBox } ->
                                                    let
                                                        { width, height, x1, y1 } =
                                                            boundingBox

                                                        ( initialMx1, initialMy1 ) =
                                                            initialMousePos

                                                        ( mx1, my1 ) =
                                                            mousePos

                                                        ( gx, gy ) =
                                                            model.mapPanOffset

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
                                            |> Maybe.withDefault (S.text "")

                                    _ ->
                                        none

                            noGroupBeingHighlightedRooms : List Room -> List Room
                            noGroupBeingHighlightedRooms initialRooms =
                                case selected of
                                    GroupSelected rooms ->
                                        initialRooms |> List.filter (\r -> not <| List.member r.id rooms)

                                    _ ->
                                        case state of
                                            DraggingToSelectMany { start, end } ->
                                                let
                                                    ( x1, y1 ) =
                                                        start

                                                    ( x2, y2 ) =
                                                        end

                                                    draggingArea =
                                                        { x1 = x1
                                                        , y1 = y1
                                                        , width = x2 - x1
                                                        , height = y2 - y1
                                                        }
                                                in
                                                initialRooms |> List.filter (\r -> not <| Rect.isInside draggingArea r.boundingBox)

                                            _ ->
                                                initialRooms

                            drawSelectionArea : List (Svg Msg)
                            drawSelectionArea =
                                case state of
                                    DraggingToSelectMany { start, end } ->
                                        let
                                            ( x1, y1 ) =
                                                start

                                            ( x2, y2 ) =
                                                end

                                            ( gx, gy ) =
                                                model.mapPanOffset
                                        in
                                        [ drawRect
                                            { x1 = x1 - gx
                                            , y1 = y1 - gy
                                            , height = y2 - y1
                                            , width = x2 - x1
                                            }
                                            [ strokeWidth "2"

                                            -- TODO: colors could be nicer
                                            , stroke "rgba(87,121,0,0.5)"
                                            , fill "rgba(36,49,4,0.5)"
                                            ]
                                        ]

                                    _ ->
                                        []
                        in
                        bgGrid
                            ++ drawRooms
                                (model.rooms
                                    |> noGroupBeingHighlightedRooms
                                 -- |> noSelectedRooms
                                 -- |> noHoveredOverRoom
                                 -- |> noDraggingRoom
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
                                    |> noDraggingRoom
                                )
                            ++ drawRoomBeingDragged model.rooms
                            :: drawSelectionArea
                 )
                    ++ drawSnapLines
                )
            ]
        , div
            [ style "position" "absolute"
            , style "top" "50%"
            , style "bottom" "50%"
            , style "right" "0"
            , style "transform" "translate(0, -50%)"
            ]
            (case model.mode of
                Delete ->
                    []

                Pan ->
                    []

                Draw _ ->
                    []

                Select { selected } ->
                    case selected of
                        NoRoomSelected ->
                            []

                        RoomSelected roomId ->
                            let
                                room =
                                    List.filter (\{ id } -> id == roomId) model.rooms
                                        |> List.head
                            in
                            case room of
                                Just { name } ->
                                    [ div
                                        [ style "background-color" "white"
                                        , style "padding" "15px"
                                        , style "display" "flex"
                                        , style "flex-direction" "column"
                                        ]
                                        [ div [] [ text "Name:", text nbsp, text name.value ]
                                        , input [ value name.value, onInput (\l -> OnChangeRectangleName ( roomId, l )) ] []
                                        ]
                                    ]

                                Nothing ->
                                    []

                        GroupSelected _ ->
                            []
            )

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
        |> List.filter predicate
        |> List.head


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
