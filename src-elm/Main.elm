port module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, preventDefaultOn)
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
    Sub.none



-- receiveChangedSvgTextContent
--     (\l ->
--         case JD.decodeValue receiveChangedSvgTextContentDecoder l of
--             Ok msg ->
--                 Debug.todo "received text svg change"
--             Err err ->
--                 Debug.todo "received text svg change"
--     )
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
    , panning : Maybe ( Point, Point )
    , mode : Mode
    , rooms : List Room
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { viewport = ( 0, 0 )
      , panning = Nothing
      , mode = Draw NotDrawing
      , rooms = []
      }
    , Cmd.none
    )



-- UPDATE


type Mode
    = Draw DrawState
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
    | RoomSelected { roomId : RoomID, resizableKind : ResizeRoom }
    | GroupSelected (List RoomID)


type ResizeRoom
    = CannotResize
    | CanResize Resizable
    | Resizing ( Resizable, { origin : Point, end : Point } )


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
    | DraggingDraw
        { start : Point
        , end : Point
        , isOverlappingAnotherRoom : Bool
        }


type Msg
    = MiddleClickDown MouseEvent
    | MouseDown MouseEvent
    | MouseMove MouseEvent
    | MouseUp MouseEvent
    | DrawMode
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

        canResizeSelectedRoom : Maybe ( RoomID, Resizable )
        canResizeSelectedRoom =
            case model.mode of
                Select { selected } ->
                    case selected of
                        RoomSelected { roomId, resizableKind } ->
                            case resizableKind of
                                CanResize resizable ->
                                    Just ( roomId, resizable )

                                _ ->
                                    Nothing

                        GroupSelected _ ->
                            -- TODO:
                            Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        isResizingSelectedRoom : Maybe ( RoomID, Resizable, { origin : Point, end : Point } )
        isResizingSelectedRoom =
            case model.mode of
                Select { selected } ->
                    case selected of
                        RoomSelected { roomId, resizableKind } ->
                            case resizableKind of
                                Resizing ( resizable, resPoints ) ->
                                    Just ( roomId, resizable, resPoints )

                                _ ->
                                    Nothing

                        GroupSelected _ ->
                            -- TODO:
                            Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
    case msg of
        DeleteMode ->
            changeMode Delete |> pure

        DrawMode ->
            changeMode (Draw NotDrawing) |> pure

        SelectMode ->
            changeMode (Select { selected = NoRoomSelected, state = NotHoveringOverRoom })
                |> pure

        MouseDown mouseEvent ->
            case model.panning of
                Just _ ->
                    ignore

                Nothing ->
                    let
                        mouseDownRelCoords =
                            ( mouseEvent.layerX, mouseEvent.layerY )
                    in
                    case model.mode of
                        Delete ->
                            ignore

                        Draw state ->
                            case state of
                                NotDrawing ->
                                    changeMode (Draw HoldingClick) |> pure

                                HoldingClick ->
                                    ignore

                                DraggingDraw _ ->
                                    ignore

                        Select { selected, state } ->
                            case canResizeSelectedRoom of
                                Just ( roomSelectedId, resKind ) ->
                                    changeMode
                                        (Select
                                            { selected =
                                                RoomSelected
                                                    { roomId = roomSelectedId
                                                    , resizableKind = Resizing ( resKind, { origin = mouseDownRelCoords, end = mouseDownRelCoords } )
                                                    }
                                            , state = state
                                            }
                                        )
                                        |> pure

                                Nothing ->
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

        MouseMove mouseEvent ->
            let
                mouseMoveRelCoords =
                    ( mouseEvent.layerX, mouseEvent.layerY )

                pan m =
                    case model.panning of
                        Nothing ->
                            m

                        Just ( origin, _ ) ->
                            { m | panning = Just ( origin, mouseMoveRelCoords ) }
            in
            (case model.mode of
                Delete ->
                    ignore

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
                                        |> List.any (.boundingBox >> Rect.isThereAnyOverlap sceneRectangle)
                            in
                            changeMode (Draw (DraggingDraw { start = mouseMoveRelCoords, end = mouseMoveRelCoords, isOverlappingAnotherRoom = isOverlappingAnotherRoom }))
                                |> pure

                        DraggingDraw { start } ->
                            let
                                sceneRectangle : Rectangle
                                sceneRectangle =
                                    pointsToRectangle start mouseMoveRelCoords
                                        |> Rect.addPosition model.viewport

                                isOverlappingAnotherRoom : Bool
                                isOverlappingAnotherRoom =
                                    model.rooms
                                        |> List.any (.boundingBox >> Rect.isThereAnyOverlap sceneRectangle)
                            in
                            changeMode (Draw (DraggingDraw { start = start, end = mouseMoveRelCoords, isOverlappingAnotherRoom = isOverlappingAnotherRoom }))
                                |> pure

                Select { selected, state } ->
                    case isResizingSelectedRoom of
                        Just ( roomSelectedId, resKind, resPoints ) ->
                            changeMode
                                (Select
                                    { selected =
                                        RoomSelected
                                            { roomId = roomSelectedId

                                            -- handle negative
                                            , resizableKind = Resizing ( resKind, { origin = resPoints.origin, end = mouseMoveRelCoords } )
                                            }
                                    , state = state
                                    }
                                )
                                |> pure

                        Nothing ->
                            let
                                deltaPan =
                                    case model.panning of
                                        Just ( origin, end ) ->
                                            end |> Point.subtract origin

                                        Nothing ->
                                            ( 0, 0 )

                                sceneMouseMoveCoords =
                                    mouseMoveRelCoords
                                        |> Point.add model.viewport
                                        |> Point.subtract deltaPan

                                roomImHoveringOver : Maybe RoomID
                                roomImHoveringOver =
                                    model.rooms
                                        |> getFirstRoom (sceneMouseMoveCoords |> isOnRoom)
                                        |> Maybe.map .id

                                checkIfCanResizeSelectedRoom : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                                checkIfCanResizeSelectedRoom ( a, b ) =
                                    case a.mode of
                                        Select selectState ->
                                            let
                                                st =
                                                    selectState.state
                                            in
                                            ( { a
                                                | mode =
                                                    Select
                                                        { selected =
                                                            case selected of
                                                                RoomSelected { roomId } ->
                                                                    case roomImHoveringOver of
                                                                        Nothing ->
                                                                            RoomSelected
                                                                                { roomId = roomId
                                                                                , resizableKind = CannotResize
                                                                                }

                                                                        Just val ->
                                                                            if val /= roomId then
                                                                                RoomSelected
                                                                                    { roomId = roomId
                                                                                    , resizableKind = CannotResize
                                                                                    }

                                                                            else
                                                                                let
                                                                                    ( x1, y1 ) =
                                                                                        sceneMouseMoveCoords

                                                                                    leeway =
                                                                                        15

                                                                                    isMouseAroundTopSideOfRoom =
                                                                                        List.any
                                                                                            (\{ boundingBox } ->
                                                                                                (y1 |> inRangeInc (boundingBox.y1 - leeway) (boundingBox.y1 + leeway))
                                                                                                    && (x1 |> inRangeInc (boundingBox.x1 + leeway) (boundingBox.x1 + boundingBox.width - leeway))
                                                                                            )
                                                                                            model.rooms

                                                                                    isMouseAroundBottomSideOfRoom =
                                                                                        List.any
                                                                                            (\{ boundingBox } ->
                                                                                                (y1 |> inRangeInc (boundingBox.y1 + boundingBox.height - leeway) (boundingBox.y1 + boundingBox.height + leeway))
                                                                                                    && (x1 |> inRangeInc (boundingBox.x1 + leeway) (boundingBox.x1 + boundingBox.width - leeway))
                                                                                            )
                                                                                            model.rooms

                                                                                    isMouseAroundLeftSideOfRoom =
                                                                                        List.any
                                                                                            (\{ boundingBox } ->
                                                                                                (x1 |> inRangeInc (boundingBox.x1 - leeway) (boundingBox.x1 + leeway))
                                                                                                    && (y1 |> inRangeInc (boundingBox.y1 + leeway) (boundingBox.y1 + boundingBox.height - leeway))
                                                                                            )
                                                                                            model.rooms

                                                                                    isMouseAroundRightSideOfRoom =
                                                                                        List.any
                                                                                            (\{ boundingBox } ->
                                                                                                (x1 |> inRangeInc (boundingBox.x1 + boundingBox.width - leeway) (boundingBox.x1 + boundingBox.width + leeway))
                                                                                                    && (y1 |> inRangeInc (boundingBox.y1 + leeway) (boundingBox.y1 + boundingBox.height - leeway))
                                                                                            )
                                                                                            model.rooms
                                                                                in
                                                                                RoomSelected
                                                                                    { roomId = roomId
                                                                                    , resizableKind =
                                                                                        if isMouseAroundTopSideOfRoom then
                                                                                            CanResize Top

                                                                                        else if isMouseAroundBottomSideOfRoom then
                                                                                            CanResize Bottom

                                                                                        else if isMouseAroundLeftSideOfRoom then
                                                                                            CanResize Left

                                                                                        else if isMouseAroundRightSideOfRoom then
                                                                                            CanResize Right

                                                                                        else
                                                                                            CannotResize
                                                                                    }

                                                                GroupSelected ids ->
                                                                    GroupSelected ids

                                                                NoRoomSelected ->
                                                                    NoRoomSelected
                                                        , state = st
                                                        }
                                              }
                                            , b
                                            )

                                        _ ->
                                            ( a, b )
                            in
                            (case state of
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
                                        deltaDrag : Point
                                        deltaDrag =
                                            dragEnd |> Point.subtract dragOrigin

                                        roomsMinusDragging =
                                            model.rooms |> List.filter (\e -> not <| List.any (\l -> e.id == l) rooms)

                                        anySelectedRoomIsOverlappingARoom : Bool
                                        anySelectedRoomIsOverlappingARoom =
                                            listAnyIJDiff
                                                (\r id -> (r.id == id) && isRoomOverlappingAnyRooms roomsMinusDragging (r |> roomAddPosition deltaDrag))
                                                model.rooms
                                                rooms
                                    in
                                    changeMode
                                        (Select
                                            { selected = selected
                                            , state =
                                                DraggingRooms
                                                    { rooms = rooms
                                                    , dragOrigin = dragOrigin
                                                    , dragEnd = mouseMoveRelCoords |> toGlobal model.viewport
                                                    , isOverlappingAnotherRoom = anySelectedRoomIsOverlappingARoom
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
                                            dragEnd |> Point.subtract dragOrigin

                                        roomsMinusDraggingRoom =
                                            model.rooms |> List.filter (\e -> e.id /= room)

                                        selectedRoomIsOverlappingAnyRoom : Bool
                                        selectedRoomIsOverlappingAnyRoom =
                                            List.any
                                                (\r -> (r.id == room) && isRoomOverlappingAnyRooms roomsMinusDraggingRoom (r |> roomAddPosition deltaDrag))
                                                model.rooms
                                    in
                                    changeMode
                                        (Select
                                            { selected = selected
                                            , state =
                                                DraggingRoom
                                                    { room = room
                                                    , dragOrigin = dragOrigin
                                                    , dragEnd = sceneMouseMoveCoords
                                                    , isOverlappingAnotherRoom = selectedRoomIsOverlappingAnyRoom
                                                    }
                                            }
                                        )
                                        |> pure
                            )
                                |> checkIfCanResizeSelectedRoom
            )
                |> (\( a, b ) -> ( pan a, b ))

        MouseUp mouseEvent ->
            case model.panning of
                Just _ ->
                    case mouseEvent.button of
                        MiddleClick ->
                            case model.panning of
                                Nothing ->
                                    ignore

                                Just ( origin, end ) ->
                                    let
                                        deltaPan =
                                            end |> Point.subtract origin
                                    in
                                    { model | viewport = model.viewport |> Point.subtract deltaPan, panning = Nothing }
                                        |> pure

                        _ ->
                            ignore

                Nothing ->
                    case mouseEvent.button of
                        MiddleClick ->
                            ignore

                        _ ->
                            let
                                mouseUpRelCoords =
                                    ( mouseEvent.layerX, mouseEvent.layerY )
                            in
                            case model.mode of
                                Select { selected, state } ->
                                    case isResizingSelectedRoom of
                                        Just ( roomImResizing, resKind, resPoints ) ->
                                            let
                                                { origin, end } =
                                                    resPoints

                                                deltaResizeDrag =
                                                    end |> Point.subtract origin
                                            in
                                            { model
                                                | rooms =
                                                    List.map
                                                        (\r ->
                                                            if r.id == roomImResizing then
                                                                let
                                                                    bBox =
                                                                        r.boundingBox

                                                                    newBBox =
                                                                        case resKind of
                                                                            Bottom ->
                                                                                let
                                                                                    h =
                                                                                        bBox.height + Point.y deltaResizeDrag

                                                                                    newH =
                                                                                        if h <= 1 then
                                                                                            abs (bBox.height + Point.y deltaResizeDrag)

                                                                                        else
                                                                                            h

                                                                                    y1 =
                                                                                        if h <= 0 then
                                                                                            bBox.y1 + h

                                                                                        else
                                                                                            bBox.y1
                                                                                in
                                                                                { bBox
                                                                                    | height = newH
                                                                                    , y1 = y1
                                                                                }

                                                                            Top ->
                                                                                let
                                                                                    y1 =
                                                                                        bBox.y1 + Point.y deltaResizeDrag

                                                                                    newY1 =
                                                                                        if y1 >= bBox.y1 + bBox.height then
                                                                                            bBox.y1 + bBox.height - 1

                                                                                        else
                                                                                            y1

                                                                                    h =
                                                                                        bBox.height - Point.y deltaResizeDrag

                                                                                    newH =
                                                                                        if h <= 1 then
                                                                                            abs (bBox.height - Point.y deltaResizeDrag)

                                                                                        else
                                                                                            h
                                                                                in
                                                                                { bBox
                                                                                    | height = newH
                                                                                    , y1 = newY1
                                                                                }

                                                                            Left ->
                                                                                let
                                                                                    x1 =
                                                                                        bBox.x1 + Point.x deltaResizeDrag

                                                                                    newX1 =
                                                                                        if x1 >= bBox.x1 + bBox.width then
                                                                                            bBox.x1 + bBox.width - 1

                                                                                        else
                                                                                            x1

                                                                                    w =
                                                                                        bBox.width - Point.x deltaResizeDrag

                                                                                    newW =
                                                                                        if w <= 1 then
                                                                                            abs (bBox.width - Point.x deltaResizeDrag)

                                                                                        else
                                                                                            w
                                                                                in
                                                                                { bBox
                                                                                    | width = newW
                                                                                    , x1 = newX1
                                                                                }

                                                                            Right ->
                                                                                let
                                                                                    w =
                                                                                        bBox.width + Point.x deltaResizeDrag

                                                                                    newW =
                                                                                        if w <= 1 then
                                                                                            abs (bBox.width + Point.x deltaResizeDrag)

                                                                                        else
                                                                                            w

                                                                                    x1 =
                                                                                        if w <= 0 then
                                                                                            bBox.x1 + w

                                                                                        else
                                                                                            bBox.x1
                                                                                in
                                                                                { bBox
                                                                                    | width = newW
                                                                                    , x1 = x1
                                                                                }
                                                                in
                                                                { r | boundingBox = newBBox }

                                                            else
                                                                r
                                                        )
                                                        model.rooms
                                                , mode =
                                                    Select
                                                        { selected = RoomSelected { roomId = roomImResizing, resizableKind = CanResize resKind }
                                                        , state = state
                                                        }
                                            }
                                                |> pure

                                        Nothing ->
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
                                                                            RoomSelected { roomId = room.id, resizableKind = CannotResize }

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
                                                                    RoomSelected r ->
                                                                        let
                                                                            roomImEditing =
                                                                                r.roomId
                                                                        in
                                                                        if roomImEditing == roomId then
                                                                            NoRoomSelected

                                                                        else
                                                                            RoomSelected { roomId = roomId, resizableKind = r.resizableKind }

                                                                    GroupSelected _ ->
                                                                        RoomSelected { roomId = roomId, resizableKind = CannotResize }

                                                                    NoRoomSelected ->
                                                                        RoomSelected { roomId = roomId, resizableKind = CannotResize }
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
                                                                                        dragEnd |> Point.subtract dragOrigin

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
                                                            dragEnd |> Point.subtract dragOrigin

                                                        globalMouseUpCoords =
                                                            mouseUpRelCoords |> toGlobal model.viewport

                                                        roomImHoveringOver : Maybe RoomID
                                                        roomImHoveringOver =
                                                            model.rooms
                                                                |> getFirstRoom (globalMouseUpCoords |> isOnRoom)
                                                                |> Maybe.map .id

                                                        roomsMinusDragging =
                                                            model.rooms |> List.filter (\e -> not <| List.any (\l -> e.id == l) rooms)

                                                        anySelectedRoomIsOverlappingARoom : Bool
                                                        anySelectedRoomIsOverlappingARoom =
                                                            listAnyIJDiff
                                                                (\r id -> (r.id == id) && isRoomOverlappingAnyRooms roomsMinusDragging (r |> roomAddPosition deltaDrag))
                                                                model.rooms
                                                                rooms
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

                                        DraggingDraw { start, end, isOverlappingAnotherRoom } ->
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

        MiddleClickDown mouseEvent ->
            let
                pan =
                    let
                        middleClickDownRelCoords =
                            ( mouseEvent.layerX, mouseEvent.layerY )
                    in
                    { model | panning = Just ( middleClickDownRelCoords, middleClickDownRelCoords ) }
                        |> pure
            in
            case model.mode of
                Select { state } ->
                    case state of
                        NotHoveringOverRoom ->
                            pan

                        HoveringOverRoom _ ->
                            pan

                        _ ->
                            ignore

                Draw state ->
                    case state of
                        NotDrawing ->
                            pan

                        _ ->
                            ignore

                Delete ->
                    pan


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
            [ style "background-color" background
            , style "width" "100vw"
            , style "height" "100vh"
            , style "position" "relative"
            , preventDefaultOn "mousedown"
                (mouseEventDecoder
                    |> JD.map
                        (\l ->
                            case l.button of
                                MiddleClick ->
                                    ( MiddleClickDown l, True )

                                _ ->
                                    ( MouseDown l, False )
                        )
                )

            -- TODO: i don't always need mousemove, refactor at the end
            -- so it's only there when i need it
            , on "mousemove" (mouseEventDecoder |> JD.map MouseMove)
            , on "mouseup" (mouseEventDecoder |> JD.map MouseUp)
            , let
                isPanning =
                    case model.panning of
                        Nothing ->
                            False

                        Just _ ->
                            True

                canOrIsResizing : Bool
                canOrIsResizing =
                    case model.mode of
                        Select { selected } ->
                            case selected of
                                RoomSelected { resizableKind } ->
                                    case resizableKind of
                                        CannotResize ->
                                            False

                                        CanResize _ ->
                                            True

                                        Resizing _ ->
                                            True

                                _ ->
                                    False

                        _ ->
                            False
              in
              if isPanning then
                style "cursor" "grabbing"

              else if canOrIsResizing then
                style "cursor"
                    (case model.mode of
                        Select { selected } ->
                            case selected of
                                RoomSelected { resizableKind } ->
                                    case resizableKind of
                                        CanResize resKind ->
                                            case resKind of
                                                Top ->
                                                    "row-resize"

                                                Bottom ->
                                                    "row-resize"

                                                Left ->
                                                    "col-resize"

                                                Right ->
                                                    "col-resize"

                                        Resizing ( resKind, _ ) ->
                                            case resKind of
                                                Top ->
                                                    "row-resize"

                                                Bottom ->
                                                    "row-resize"

                                                Left ->
                                                    "col-resize"

                                                Right ->
                                                    "col-resize"

                                        _ ->
                                            ""

                                _ ->
                                    ""

                        _ ->
                            ""
                    )

              else
                style "" ""
            ]
            -- NOTE: Drawing order is bottom to top, draw last on top
            [ svg [ version "1.1", SA.width (screenWidth |> String.fromInt), SA.height (screenHeight |> String.fromInt), viewBox "0 0 1900 800" ]
                -- DRAW
                (let
                    viewport : Point
                    viewport =
                        case model.panning of
                            Nothing ->
                                model.viewport

                            Just ( origin, end ) ->
                                let
                                    panDelta =
                                        end |> Point.subtract origin
                                in
                                model.viewport |> Point.subtract panDelta

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

                    topMostRects =
                        case model.mode of
                            Select { state, selected } ->
                                case state of
                                    DraggingRoom { room, dragOrigin, dragEnd } ->
                                        getAllRoomsTopXAsSegments
                                            (model.rooms
                                                |> List.map
                                                    (\e ->
                                                        if e.id == room then
                                                            let
                                                                deltaDrag : Point
                                                                deltaDrag =
                                                                    dragEnd |> Point.subtract dragOrigin

                                                                newDraggedRoom =
                                                                    e
                                                                        |> roomAddPosition deltaDrag

                                                                draggedRoomAfterSnapping : Room
                                                                draggedRoomAfterSnapping =
                                                                    (model.rooms |> List.filter (\r -> r.id /= room) |> List.map (roomSubPosition model.viewport))
                                                                        |> handleSnapping newDraggedRoom
                                                                        |> handleTranslateRoomToSnappedPosition newDraggedRoom
                                                            in
                                                            draggedRoomAfterSnapping

                                                        else
                                                            e
                                                    )
                                            )

                                    _ ->
                                        let
                                            -- TODO: refactor this function into as standalone
                                            beingResizedRooms =
                                                case selected of
                                                    RoomSelected { roomId, resizableKind } ->
                                                        case resizableKind of
                                                            Resizing ( kind, resizablePoints ) ->
                                                                let
                                                                    { origin, end } =
                                                                        resizablePoints

                                                                    deltaResizeDrag =
                                                                        end |> Point.subtract origin
                                                                in
                                                                model.rooms
                                                                    |> List.map
                                                                        (\r ->
                                                                            if r.id == roomId then
                                                                                let
                                                                                    bBox =
                                                                                        r.boundingBox

                                                                                    newBBox =
                                                                                        case kind of
                                                                                            Bottom ->
                                                                                                let
                                                                                                    h =
                                                                                                        bBox.height + Point.y deltaResizeDrag

                                                                                                    newH =
                                                                                                        if h <= 1 then
                                                                                                            abs (bBox.height + Point.y deltaResizeDrag)

                                                                                                        else
                                                                                                            h

                                                                                                    y1 =
                                                                                                        if h <= 0 then
                                                                                                            bBox.y1 + h

                                                                                                        else
                                                                                                            bBox.y1
                                                                                                in
                                                                                                { bBox
                                                                                                    | height = newH
                                                                                                    , y1 = y1
                                                                                                }

                                                                                            Top ->
                                                                                                let
                                                                                                    y1 =
                                                                                                        bBox.y1 + Point.y deltaResizeDrag

                                                                                                    newY1 =
                                                                                                        if y1 >= bBox.y1 + bBox.height then
                                                                                                            bBox.y1 + bBox.height - 1

                                                                                                        else
                                                                                                            y1

                                                                                                    h =
                                                                                                        bBox.height - Point.y deltaResizeDrag

                                                                                                    newH =
                                                                                                        if h <= 1 then
                                                                                                            abs (bBox.height - Point.y deltaResizeDrag)

                                                                                                        else
                                                                                                            h
                                                                                                in
                                                                                                { bBox
                                                                                                    | height = newH
                                                                                                    , y1 = newY1
                                                                                                }

                                                                                            Left ->
                                                                                                let
                                                                                                    x1 =
                                                                                                        bBox.x1 + Point.x deltaResizeDrag

                                                                                                    newX1 =
                                                                                                        if x1 >= bBox.x1 + bBox.width then
                                                                                                            bBox.x1 + bBox.width - 1

                                                                                                        else
                                                                                                            x1

                                                                                                    w =
                                                                                                        bBox.width - Point.x deltaResizeDrag

                                                                                                    newW =
                                                                                                        if w <= 1 then
                                                                                                            abs (bBox.width - Point.x deltaResizeDrag)

                                                                                                        else
                                                                                                            w
                                                                                                in
                                                                                                { bBox
                                                                                                    | width = newW
                                                                                                    , x1 = newX1
                                                                                                }

                                                                                            Right ->
                                                                                                let
                                                                                                    w =
                                                                                                        bBox.width + Point.x deltaResizeDrag

                                                                                                    newW =
                                                                                                        if w <= 1 then
                                                                                                            abs (bBox.width + Point.x deltaResizeDrag)

                                                                                                        else
                                                                                                            w

                                                                                                    x1 =
                                                                                                        if w <= 0 then
                                                                                                            bBox.x1 + w

                                                                                                        else
                                                                                                            bBox.x1
                                                                                                in
                                                                                                { bBox
                                                                                                    | width = newW
                                                                                                    , x1 = x1
                                                                                                }
                                                                                in
                                                                                { r | boundingBox = newBBox }

                                                                            else
                                                                                r
                                                                        )

                                                            _ ->
                                                                model.rooms

                                                    _ ->
                                                        model.rooms
                                        in
                                        getAllRoomsTopXAsSegments beingResizedRooms

                            _ ->
                                getAllRoomsTopXAsSegments model.rooms

                    drawMeasuringLines =
                        case topMostRects of
                            [] ->
                                []

                            x :: xs ->
                                let
                                    smallestY =
                                        List.foldl
                                            (\next acc ->
                                                case next of
                                                    EmptySpace _ ->
                                                        acc

                                                    Occupied data ->
                                                        if data.y1 < acc then
                                                            data.y1

                                                        else
                                                            acc
                                            )
                                            -- TEMP sorta
                                            50000
                                            (x :: xs)

                                    smallestX =
                                        List.foldl
                                            (\next acc ->
                                                case next of
                                                    EmptySpace _ ->
                                                        acc

                                                    Occupied data ->
                                                        if data.x1 < acc then
                                                            data.x1

                                                        else
                                                            acc
                                            )
                                            -- TODO: temp
                                            50000
                                            (x :: xs)

                                    biggestX =
                                        List.foldl
                                            (\next acc ->
                                                case next of
                                                    EmptySpace _ ->
                                                        acc

                                                    Occupied data ->
                                                        if data.x1 + data.width > acc then
                                                            data.x1 + data.width

                                                        else
                                                            acc
                                            )
                                            -- TODO: temp
                                            -50000
                                            (x :: xs)

                                    ( vx, vy ) =
                                        viewport
                                in
                                List.map
                                    (\l ->
                                        case l of
                                            EmptySpace data ->
                                                S.g []
                                                    [ line
                                                        [ SA.x1 (data.x - vx |> String.fromInt)
                                                        , SA.y1 (smallestY - vy - 50 |> String.fromInt)
                                                        , SA.x2 (data.x - vx |> String.fromInt)
                                                        , SA.y2 (smallestY - vy - 25 |> String.fromInt)
                                                        , SA.stroke "orange"
                                                        , SA.strokeWidth "2"
                                                        ]
                                                        []
                                                    , line
                                                        [ SA.x1 (data.x + data.width - vx |> String.fromInt)
                                                        , SA.y1 (smallestY - vy - 75 |> String.fromInt)
                                                        , SA.x2 (data.x + data.width - vx |> String.fromInt)
                                                        , SA.y2 (smallestY - vy - 50 |> String.fromInt)
                                                        , SA.stroke "DarkTurquoise"
                                                        , SA.strokeWidth "2"
                                                        ]
                                                        []
                                                    , S.text_
                                                        [ SA.x (data.x - vx |> String.fromInt)
                                                        , SA.y (smallestY - vy - 55 |> String.fromInt)
                                                        , SA.fill "white"
                                                        , SA.class "svgText"
                                                        ]
                                                        [ S.text (data.width |> String.fromInt)
                                                        ]
                                                    ]

                                            Occupied { x1, width } ->
                                                S.g []
                                                    [ line
                                                        [ SA.x1 (x1 - vx |> String.fromInt)
                                                        , SA.y1 (smallestY - vy - 50 |> String.fromInt)
                                                        , SA.x2 (x1 - vx |> String.fromInt)
                                                        , SA.y2 (smallestY - vy - 25 |> String.fromInt)
                                                        , SA.stroke "Chartreuse"
                                                        , SA.strokeWidth "2"
                                                        ]
                                                        []
                                                    , line
                                                        [ SA.x1 (x1 + width - vx |> String.fromInt)
                                                        , SA.y1 (smallestY - vy - 75 |> String.fromInt)
                                                        , SA.x2 (x1 + width - vx |> String.fromInt)
                                                        , SA.y2 (smallestY - vy - 50 |> String.fromInt)
                                                        , SA.stroke "red"
                                                        , SA.strokeWidth "2"
                                                        ]
                                                        []
                                                    , S.text_
                                                        [ SA.x (x1 - vx |> String.fromInt)
                                                        , SA.y (smallestY - vy - 55 |> String.fromInt)
                                                        , SA.fill "white"
                                                        , SA.class "svgText"
                                                        ]
                                                        [ S.text (width |> String.fromInt)
                                                        ]
                                                    ]
                                    )
                                    topMostRects
                                    ++ [ line
                                            [ x1 (smallestX - vx |> String.fromInt)
                                            , y1 (smallestY - vy - 50 |> String.fromInt)
                                            , x2 (biggestX - vx |> String.fromInt)
                                            , y2 (smallestY - vy - 50 |> String.fromInt)
                                            , stroke "orange"
                                            , strokeWidth "2"
                                            ]
                                            []
                                       ]
                 in
                 -- NOTE: Drawing order is top to bottom, draw on top last
                 case model.mode of
                    Delete ->
                        bgGrid
                            ++ drawRooms model.rooms
                            ++ drawMeasuringLines

                    Draw state ->
                        bgGrid
                            ++ (case state of
                                    NotDrawing ->
                                        drawRooms model.rooms

                                    HoldingClick ->
                                        drawRooms model.rooms

                                    DraggingDraw { start, end, isOverlappingAnotherRoom } ->
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
                            ++ drawMeasuringLines

                    Select { selected, state } ->
                        let
                            transformRoomsBeingResized : List Room -> List Room
                            transformRoomsBeingResized rooms =
                                case selected of
                                    RoomSelected { roomId, resizableKind } ->
                                        case resizableKind of
                                            Resizing ( kind, resizablePoints ) ->
                                                let
                                                    { origin, end } =
                                                        resizablePoints

                                                    deltaResizeDrag =
                                                        end |> Point.subtract origin
                                                in
                                                rooms
                                                    |> List.map
                                                        (\r ->
                                                            if r.id == roomId then
                                                                let
                                                                    bBox =
                                                                        r.boundingBox

                                                                    newBBox =
                                                                        case kind of
                                                                            -- TODO: can be 0 height
                                                                            Bottom ->
                                                                                let
                                                                                    h =
                                                                                        bBox.height + Point.y deltaResizeDrag

                                                                                    newH =
                                                                                        if h <= 1 then
                                                                                            abs (bBox.height + Point.y deltaResizeDrag)

                                                                                        else
                                                                                            h

                                                                                    y1 =
                                                                                        if h <= 0 then
                                                                                            bBox.y1 + h

                                                                                        else
                                                                                            bBox.y1
                                                                                in
                                                                                { bBox
                                                                                    | height = newH
                                                                                    , y1 = y1
                                                                                }

                                                                            Top ->
                                                                                let
                                                                                    y1 =
                                                                                        bBox.y1 + Point.y deltaResizeDrag

                                                                                    newY1 =
                                                                                        if y1 >= bBox.y1 + bBox.height then
                                                                                            bBox.y1 + bBox.height - 1

                                                                                        else
                                                                                            y1

                                                                                    h =
                                                                                        bBox.height - Point.y deltaResizeDrag

                                                                                    newH =
                                                                                        if h <= 1 then
                                                                                            abs (bBox.height - Point.y deltaResizeDrag)

                                                                                        else
                                                                                            h
                                                                                in
                                                                                { bBox
                                                                                    | height = newH
                                                                                    , y1 = newY1
                                                                                }

                                                                            Left ->
                                                                                let
                                                                                    x1 =
                                                                                        bBox.x1 + Point.x deltaResizeDrag

                                                                                    newX1 =
                                                                                        if x1 >= bBox.x1 + bBox.width then
                                                                                            bBox.x1 + bBox.width - 1

                                                                                        else
                                                                                            x1

                                                                                    w =
                                                                                        bBox.width - Point.x deltaResizeDrag

                                                                                    newW =
                                                                                        if w <= 1 then
                                                                                            abs (bBox.width - Point.x deltaResizeDrag)

                                                                                        else
                                                                                            w
                                                                                in
                                                                                { bBox
                                                                                    | width = newW
                                                                                    , x1 = newX1
                                                                                }

                                                                            Right ->
                                                                                let
                                                                                    w =
                                                                                        bBox.width + Point.x deltaResizeDrag

                                                                                    newW =
                                                                                        if w <= 1 then
                                                                                            abs (bBox.width + Point.x deltaResizeDrag)

                                                                                        else
                                                                                            w

                                                                                    x1 =
                                                                                        if w <= 0 then
                                                                                            bBox.x1 + w

                                                                                        else
                                                                                            bBox.x1
                                                                                in
                                                                                { bBox
                                                                                    | width = newW
                                                                                    , x1 = x1
                                                                                }
                                                                in
                                                                { r | boundingBox = newBBox }

                                                            else
                                                                r
                                                        )

                                            _ ->
                                                rooms

                                    _ ->
                                        rooms

                            noSelectedRooms : List Room -> List Room
                            noSelectedRooms initialRooms =
                                case selected of
                                    NoRoomSelected ->
                                        initialRooms

                                    RoomSelected { roomId } ->
                                        initialRooms |> List.filter (\r -> r.id /= roomId)

                                    GroupSelected rooms ->
                                        initialRooms
                                            |> List.filter (\r -> not <| List.member r.id rooms)

                            drawHighlightedRooms : List Room -> List (Svg Msg)
                            drawHighlightedRooms rooms =
                                rooms
                                    |> transformRoomsBeingResized
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
                                                            dragEnd |> Point.subtract dragOrigin

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
                                                            dragEnd |> Point.subtract dragOrigin
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
                                                            dragEnd |> Point.subtract dragOrigin

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
                            --
                            ++ drawMeasuringLines
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
            [ button [ style "padding" "5px", onClick DrawMode ] [ text "Draw" ]
            , button [ style "padding" "5px", onClick SelectMode ] [ text "Select" ]
            , button [ style "padding" "5px", onClick DeleteMode ] [ text "Delete" ]
            ]
        ]


mouseEventDecoder : Decoder MouseEvent
mouseEventDecoder =
    -- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
    JD.map3
        (\btn x y ->
            { layerX = x
            , layerY = y
            , button = buttonToMouseEventButton btn
            }
        )
        (JD.field "button" JD.int)
        (JD.field "layerX" JD.int)
        (JD.field "layerY" JD.int)


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


type RoomPossibleSnappingX
    = SnappingXTop
    | SnappingXMiddle
    | SnappingXBottom


type RoomPossibleSnappingY
    = SnappingYLeft
    | SnappingYMiddle
    | SnappingYRight


inRangeInc : number -> number -> number -> Bool
inRangeInc min max number =
    min <= number && number <= max


inRangeExc : number -> number -> number -> Bool
inRangeExc min max number =
    min < number && number < max



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
            inRangeInc (number - snapDistanceRange) (number + snapDistanceRange) roomImChecking.boundingBox.y1

        insideTopDistance : Int -> Int
        insideTopDistance number =
            abs (number - roomImChecking.boundingBox.y1)

        isInsideMiddleSnappableArea : Int -> Bool
        isInsideMiddleSnappableArea number =
            inRangeInc (number - snapDistanceRange) (number + snapDistanceRange) (Rect.centerY roomImChecking.boundingBox)

        insideMiddleDistance : Int -> Int
        insideMiddleDistance number =
            abs (number - Rect.centerY roomImChecking.boundingBox)

        isInsideBottomSnappableArea : Int -> Bool
        isInsideBottomSnappableArea number =
            inRangeInc (number - snapDistanceRange) (number + snapDistanceRange) (Rect.bottomY roomImChecking.boundingBox)

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
            inRangeInc (number - snapDistanceRange) (number + snapDistanceRange) roomImChecking.boundingBox.x1

        insideLeftDistance : Int -> Int
        insideLeftDistance number =
            abs (number - roomImChecking.boundingBox.x1)

        isInsideMiddleSnappableArea : Int -> Bool
        isInsideMiddleSnappableArea number =
            inRangeInc (number - snapDistanceRange) (number + snapDistanceRange) (Rect.centerX roomImChecking.boundingBox)

        insideMiddleDistance : Int -> Int
        insideMiddleDistance number =
            abs (number - Rect.centerX roomImChecking.boundingBox)

        isInsideRightSnappableArea : Int -> Bool
        isInsideRightSnappableArea number =
            inRangeInc (number - snapDistanceRange) (number + snapDistanceRange) (Rect.rightX roomImChecking.boundingBox)

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
    inRangeInc a1 a2 b1 || inRangeInc a1 a2 b2 || inRangeInc b1 b2 a1 || inRangeInc b1 b2 a2


are1DLinesEq : ( number, number ) -> ( number, number ) -> Bool
are1DLinesEq ( a1, a2 ) ( b1, b2 ) =
    a1 == b1 && a2 == b2


is1DLineInside1DLine : ( Int, Int ) -> ( Int, Int ) -> Bool
is1DLineInside1DLine ( a1, a2 ) ( b1, b2 ) =
    (b1 |> inRangeInc a1 a2) && (b2 |> inRangeInc a1 a2)


isInside1DLine : ( number, number ) -> number -> Bool
isInside1DLine ( a1, a2 ) n =
    n |> inRangeInc a1 a2


isOutside1DLine : ( Int, Int ) -> ( Int, Int ) -> Bool
isOutside1DLine ( a1, a2 ) ( b1, b2 ) =
    not (b1 |> inRangeInc a1 a2) && not (b2 |> inRangeInc a1 a2)


isInside1DLines : List ( Int, Int ) -> ( Int, Int ) -> Bool
isInside1DLines lines line =
    List.any (\l -> line |> is1DLineInside1DLine l) lines


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


type SpaceType
    = Occupied Rectangle
    | EmptySpace { x : Int, width : Int }


foldlDefaultFirst : (a -> a -> a) -> List a -> Maybe a
foldlDefaultFirst fn list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (List.foldl fn x xs)


pointInsideTopLineOfRoom : Int -> Room -> Bool
pointInsideTopLineOfRoom point r =
    point |> isInside1DLine (Rect.xAs1DLine r.boundingBox)


getNextRightSegment : Rectangle -> List Room -> List SpaceType
getNextRightSegment segment rooms =
    -- There will never be a segment to the left and current segment will always be
    -- at least 1 of width
    case rooms of
        [] ->
            []

        allRooms ->
            let
                roomsAboveSegment r =
                    r.boundingBox.y1 < segment.y1

                roomsAboveAndInsideSegment : List Room
                roomsAboveAndInsideSegment =
                    allRooms
                        |> List.filter roomsAboveSegment
                        |> List.filter (\r -> (r.boundingBox |> Rect.xAs1DLine) |> is1DLineInside1DLine (segment |> Rect.xAs1DLine))

                getRoomsToTheRight =
                    let
                        roomsToTheRightOfSegment =
                            allRooms
                                |> List.filter (\r -> (segment |> Rect.rightX) < r.boundingBox.x1)
                    in
                    case roomsToTheRightOfSegment of
                        x :: xs ->
                            let
                                closestAndHighestRoomToTheRight =
                                    List.foldl
                                        (\next curr ->
                                            if next.boundingBox.x1 < curr.boundingBox.x1 then
                                                next

                                            else if next.boundingBox.x1 == curr.boundingBox.x1 then
                                                if next.boundingBox.y1 < curr.boundingBox.y1 then
                                                    next

                                                else
                                                    curr

                                            else
                                                curr
                                        )
                                        x
                                        xs
                                        |> .boundingBox
                            in
                            if closestAndHighestRoomToTheRight.x1 == ((segment |> Rect.rightX) + 1) then
                                Occupied segment :: getNextRightSegment closestAndHighestRoomToTheRight allRooms

                            else
                                [ Occupied segment
                                , EmptySpace
                                    { x = segment |> Rect.rightX
                                    , width = closestAndHighestRoomToTheRight.x1 - (segment |> Rect.rightX)
                                    }
                                ]
                                    ++ getNextRightSegment closestAndHighestRoomToTheRight allRooms

                        [] ->
                            [ Occupied segment ]
            in
            case roomsAboveAndInsideSegment of
                x :: xs ->
                    let
                        nextSegment =
                            List.foldl
                                (\next curr ->
                                    if next.boundingBox.x1 < curr.boundingBox.x1 then
                                        next

                                    else if next.boundingBox.x1 == curr.boundingBox.x1 then
                                        if next.boundingBox.y1 < curr.boundingBox.y1 then
                                            next

                                        else
                                            curr

                                    else
                                        curr
                                )
                                x
                                xs
                                |> .boundingBox

                        newCurrentSegment =
                            { x1 = segment.x1
                            , y1 = segment.y1
                            , width = nextSegment.x1 - segment.x1
                            , height = segment.height
                            }
                    in
                    Occupied newCurrentSegment :: getNextRightSegment nextSegment allRooms

                [] ->
                    let
                        segmentX2IsPartiallyInsideRoom r =
                            (segment |> Rect.rightX) |> isInside1DLine (r.boundingBox |> Rect.xAs1DLine)

                        partiallyInsideOfSegmentRooms =
                            List.filter segmentX2IsPartiallyInsideRoom allRooms
                    in
                    case partiallyInsideOfSegmentRooms of
                        s :: sx ->
                            let
                                roomsOnTop =
                                    partiallyInsideOfSegmentRooms
                                        |> List.filter roomsAboveSegment
                            in
                            case roomsOnTop of
                                x :: xs ->
                                    let
                                        nextSegment =
                                            List.foldl
                                                (\next curr ->
                                                    if next.boundingBox.x1 < curr.boundingBox.x1 then
                                                        next

                                                    else if next.boundingBox.x1 == curr.boundingBox.x1 then
                                                        if next.boundingBox.y1 < curr.boundingBox.y1 then
                                                            next

                                                        else
                                                            curr

                                                    else
                                                        curr
                                                )
                                                x
                                                xs
                                                |> .boundingBox

                                        newCurrentSegment =
                                            { x1 = segment.x1
                                            , y1 = segment.y1
                                            , width = nextSegment.x1 - segment.x1
                                            , height = segment.height
                                            }
                                    in
                                    Occupied newCurrentSegment :: getNextRightSegment nextSegment allRooms

                                [] ->
                                    let
                                        roomsOnBottom =
                                            partiallyInsideOfSegmentRooms
                                                |> List.filter (\l -> segment.y1 < l.boundingBox.y1)
                                                |> List.filter (\l -> (segment |> Rect.rightX) /= (l.boundingBox |> Rect.rightX))
                                    in
                                    case roomsOnBottom of
                                        x :: xs ->
                                            let
                                                nextSegment =
                                                    List.foldl
                                                        (\next curr ->
                                                            if next.boundingBox.y1 < curr.boundingBox.y1 then
                                                                next

                                                            else
                                                                curr
                                                        )
                                                        x
                                                        xs
                                                        |> .boundingBox
                                                        |> (\o ->
                                                                { o
                                                                    | x1 = segment |> Rect.rightX
                                                                    , width = (o |> Rect.rightX) - (segment |> Rect.rightX)
                                                                }
                                                           )
                                            in
                                            Occupied segment :: getNextRightSegment nextSegment allRooms

                                        [] ->
                                            getRoomsToTheRight

                        [] ->
                            getRoomsToTheRight


getNextTopLeftSegment : Rectangle -> List Room -> List SpaceType
getNextTopLeftSegment segment rooms =
    -- There will never be a segment to the right and current segment will always be
    -- at least 1 of width
    case rooms of
        [] ->
            []

        allRooms ->
            let
                roomsAboveSegment r =
                    r.boundingBox.y1 < segment.y1

                roomsAboveAndInsideSegment : List Room
                roomsAboveAndInsideSegment =
                    allRooms
                        |> List.filter roomsAboveSegment
                        |> List.filter (\r -> (r.boundingBox |> Rect.xAs1DLine) |> is1DLineInside1DLine (segment |> Rect.xAs1DLine))

                getRoomsToTheLeft =
                    let
                        roomsToTheLeftOfSegment =
                            allRooms
                                |> List.filter (\r -> (r.boundingBox |> Rect.rightX) < segment.x1)
                    in
                    case roomsToTheLeftOfSegment of
                        x :: xs ->
                            let
                                closestAndHighestRoomToTheLeft =
                                    List.foldl
                                        (\next curr ->
                                            if (next.boundingBox |> Rect.rightX) > (curr.boundingBox |> Rect.rightX) then
                                                next

                                            else if next.boundingBox.x1 == curr.boundingBox.x1 then
                                                if next.boundingBox.y1 < curr.boundingBox.y1 then
                                                    next

                                                else
                                                    curr

                                            else
                                                curr
                                        )
                                        x
                                        xs
                                        |> .boundingBox
                            in
                            if (closestAndHighestRoomToTheLeft |> Rect.rightX) == (segment.x1 - 1) then
                                getNextTopLeftSegment closestAndHighestRoomToTheLeft allRooms ++ [ Occupied segment ]

                            else
                                getNextTopLeftSegment closestAndHighestRoomToTheLeft allRooms
                                    ++ [ EmptySpace
                                            { x = closestAndHighestRoomToTheLeft |> Rect.rightX
                                            , width = segment.x1 - (closestAndHighestRoomToTheLeft |> Rect.rightX)
                                            }
                                       , Occupied segment
                                       ]

                        [] ->
                            [ Occupied segment ]
            in
            case roomsAboveAndInsideSegment of
                x :: xs ->
                    let
                        prevSegment =
                            List.foldl
                                (\next curr ->
                                    if (next.boundingBox |> Rect.rightX) > (curr.boundingBox |> Rect.rightX) then
                                        next

                                    else if next.boundingBox.x1 == curr.boundingBox.x1 then
                                        if next.boundingBox.y1 < curr.boundingBox.y1 then
                                            next

                                        else
                                            curr

                                    else
                                        curr
                                )
                                x
                                xs
                                |> .boundingBox

                        newCurrentSegment =
                            { x1 = prevSegment |> Rect.rightX
                            , y1 = segment.y1
                            , width = (segment |> Rect.rightX) - (prevSegment |> Rect.rightX)
                            , height = segment.height
                            }
                    in
                    getNextTopLeftSegment prevSegment allRooms ++ [ Occupied newCurrentSegment ]

                [] ->
                    let
                        segmentX1IsPartiallyInsideRoom r =
                            segment.x1 |> isInside1DLine (r.boundingBox |> Rect.xAs1DLine)

                        partiallyInsideOfSegmentRooms =
                            List.filter segmentX1IsPartiallyInsideRoom allRooms
                    in
                    case partiallyInsideOfSegmentRooms of
                        s :: sx ->
                            let
                                roomsOnTop =
                                    partiallyInsideOfSegmentRooms
                                        |> List.filter roomsAboveSegment
                            in
                            case roomsOnTop of
                                x :: xs ->
                                    let
                                        nextSegment =
                                            List.foldl
                                                (\next curr ->
                                                    if (next.boundingBox |> Rect.rightX) > (curr.boundingBox |> Rect.rightX) then
                                                        next

                                                    else if next.boundingBox.x1 == curr.boundingBox.x1 then
                                                        if next.boundingBox.y1 < curr.boundingBox.y1 then
                                                            next

                                                        else
                                                            curr

                                                    else
                                                        curr
                                                )
                                                x
                                                xs
                                                |> .boundingBox

                                        newCurrentSegment =
                                            { x1 = nextSegment |> Rect.rightX
                                            , y1 = segment.y1
                                            , width = (segment |> Rect.rightX) - (nextSegment |> Rect.rightX)
                                            , height = segment.height
                                            }
                                    in
                                    getNextTopLeftSegment nextSegment allRooms ++ [ Occupied newCurrentSegment ]

                                [] ->
                                    let
                                        ignoreThisRoom l =
                                            segment.x1 /= l.boundingBox.x1

                                        roomsOnBottom =
                                            partiallyInsideOfSegmentRooms
                                                |> List.filter (\l -> segment.y1 < l.boundingBox.y1)
                                                |> List.filter ignoreThisRoom
                                    in
                                    case roomsOnBottom of
                                        x :: xs ->
                                            let
                                                nextSegment =
                                                    List.foldl
                                                        (\next curr ->
                                                            if next.boundingBox.y1 < curr.boundingBox.y1 then
                                                                next

                                                            else
                                                                curr
                                                        )
                                                        x
                                                        xs
                                                        |> .boundingBox
                                                        |> (\o ->
                                                                { o
                                                                    | width = segment.x1 - o.x1
                                                                }
                                                           )
                                            in
                                            getNextTopLeftSegment nextSegment allRooms ++ [ Occupied segment ]

                                        [] ->
                                            getRoomsToTheLeft

                        [] ->
                            getRoomsToTheLeft


getAllRoomsTopXAsSegments : List Room -> List SpaceType
getAllRoomsTopXAsSegments e =
    case e of
        [] ->
            []

        [ onlyRoom ] ->
            [ Occupied onlyRoom.boundingBox ]

        firstRoom :: otherRooms ->
            let
                highestRoom : Room
                highestRoom =
                    List.foldl
                        (\next curr ->
                            if next.boundingBox.y1 < curr.boundingBox.y1 then
                                next

                            else
                                curr
                        )
                        firstRoom
                        otherRooms

                allRoomsMinusHighest =
                    List.filter (\r -> r.id /= highestRoom.id) (firstRoom :: otherRooms)

                belowHighest r =
                    r.boundingBox.y1 >= highestRoom.boundingBox.y1

                toTheRightOfHighest r =
                    ((highestRoom.boundingBox |> Rect.rightX) + 1) < r.boundingBox.x1

                rightNextToHighestRoom r =
                    pointInsideTopLineOfRoom ((highestRoom.boundingBox |> Rect.rightX) + 1) r

                nextOneOnTheRight : List SpaceType
                nextOneOnTheRight =
                    case
                        allRoomsMinusHighest
                            |> List.filter rightNextToHighestRoom
                            |> List.filter belowHighest
                    of
                        x :: xs ->
                            let
                                nextSegment =
                                    List.foldl
                                        (\next curr ->
                                            if next.boundingBox.y1 < curr.boundingBox.y1 then
                                                next

                                            else
                                                curr
                                        )
                                        x
                                        xs
                                        |> .boundingBox
                            in
                            if nextSegment.x1 == (highestRoom.boundingBox |> Rect.rightX) + 1 then
                                getNextRightSegment nextSegment allRoomsMinusHighest

                            else
                                getNextRightSegment
                                    { x1 = highestRoom.boundingBox |> Rect.rightX
                                    , y1 = nextSegment.y1
                                    , width = (nextSegment |> Rect.rightX) - (highestRoom.boundingBox |> Rect.rightX)
                                    , height = nextSegment.height
                                    }
                                    allRoomsMinusHighest

                        [] ->
                            case
                                allRoomsMinusHighest
                                    |> List.filter belowHighest
                                    |> List.filter toTheRightOfHighest
                                    |> foldlDefaultFirst
                                        (\next curr ->
                                            if next.boundingBox.x1 < curr.boundingBox.x1 then
                                                next

                                            else if next.boundingBox.x1 == curr.boundingBox.x1 then
                                                if next.boundingBox.y1 < curr.boundingBox.y1 then
                                                    next

                                                else
                                                    curr

                                            else
                                                curr
                                        )
                            of
                                Just roomAcross ->
                                    EmptySpace
                                        { x = (highestRoom.boundingBox |> Rect.rightX) + 1
                                        , width = roomAcross.boundingBox.x1 - (highestRoom.boundingBox |> Rect.rightX)
                                        }
                                        :: getNextRightSegment roomAcross.boundingBox allRoomsMinusHighest

                                Nothing ->
                                    []

                rightLeftToHighestRoom r =
                    pointInsideTopLineOfRoom (highestRoom.boundingBox.x1 - 1) r

                toTheLeftOfHighest r =
                    (r.boundingBox |> Rect.rightX) < (highestRoom.boundingBox.x1 - 1)

                nextOneOnTheLeft : List SpaceType
                nextOneOnTheLeft =
                    case
                        allRoomsMinusHighest
                            |> List.filter rightLeftToHighestRoom
                            |> List.filter belowHighest
                    of
                        x :: xs ->
                            let
                                prevSegment =
                                    List.foldl
                                        (\next curr ->
                                            if next.boundingBox.y1 < curr.boundingBox.y1 then
                                                next

                                            else
                                                curr
                                        )
                                        x
                                        xs
                                        |> .boundingBox
                            in
                            if (prevSegment |> Rect.rightX) == (highestRoom.boundingBox.x1 - 1) then
                                getNextTopLeftSegment prevSegment allRoomsMinusHighest

                            else
                                getNextTopLeftSegment
                                    { x1 = prevSegment.x1
                                    , y1 = prevSegment.y1
                                    , width = highestRoom.boundingBox.x1 - prevSegment.x1
                                    , height = prevSegment.height
                                    }
                                    allRoomsMinusHighest

                        [] ->
                            case
                                allRoomsMinusHighest
                                    |> List.filter belowHighest
                                    |> List.filter toTheLeftOfHighest
                                    |> foldlDefaultFirst
                                        (\next curr ->
                                            if (next.boundingBox |> Rect.rightX) > (curr.boundingBox |> Rect.rightX) then
                                                next

                                            else if next.boundingBox.x1 == curr.boundingBox.x1 then
                                                if next.boundingBox.y1 < curr.boundingBox.y1 then
                                                    next

                                                else
                                                    curr

                                            else
                                                curr
                                        )
                            of
                                Just roomAcross ->
                                    getNextTopLeftSegment roomAcross.boundingBox allRoomsMinusHighest
                                        ++ [ EmptySpace
                                                { x = roomAcross.boundingBox |> Rect.rightX
                                                , width = highestRoom.boundingBox.x1 - (roomAcross.boundingBox |> Rect.rightX)
                                                }
                                           ]

                                Nothing ->
                                    []
            in
            nextOneOnTheLeft ++ (Occupied highestRoom.boundingBox :: nextOneOnTheRight)


type SegmentPartition num
    = LineSegment ( num, num )
    | EmptySegment ( num, num )



{-
   assumes that:
   - lines have at least 1 of width
   - a1 is smaller than b1, i.e line A is more to the left then line B

   returns:
   left-to-right partitioning of the line segments
-}


ltr1DLineSegmentPartitioning : ( number, number ) -> ( number, number ) -> List (SegmentPartition number)
ltr1DLineSegmentPartitioning a b =
    let
        ( a1, b1 ) =
            a

        ( a2, b2 ) =
            b

        ( x1, x2 ) =
            ( a1, a1 + b1 )

        ( x3, x4 ) =
            ( a2, a2 + b2 )

        isInside =
            x1 < x3 && x4 <= x2

        isOutside =
            x2 < x3
    in
    if isInside then
        let
            firstSegment =
                ( x1, x3 - x1 )

            secondSegment =
                b

            thirdSegment =
                ( x4, x2 - x4 )
        in
        if x4 == x2 then
            [ LineSegment firstSegment, LineSegment secondSegment ]

        else
            [ LineSegment firstSegment, LineSegment secondSegment, LineSegment thirdSegment ]

    else if isOutside then
        [ LineSegment a, EmptySegment ( x2, x3 - x2 ), LineSegment b ]

    else
        [ LineSegment ( x1, x3 - x1 ), LineSegment b ]


areRoomsOverlapping : List Room -> List Room -> Bool
areRoomsOverlapping a b =
    listAnyIJSame (\room otherRoom -> (room.id /= otherRoom.id) && Rect.isThereAnyOverlap room.boundingBox otherRoom.boundingBox) a b


isRoomOverlappingAnyRooms : List Room -> Room -> Bool
isRoomOverlappingAnyRooms rooms room =
    List.any (\otherRoom -> (room.id /= otherRoom.id) && Rect.isThereAnyOverlap room.boundingBox otherRoom.boundingBox) rooms


listAnyIJSame : (a -> a -> Bool) -> List a -> List a -> Bool
listAnyIJSame f l1 l2 =
    List.any (\a -> List.any (\b -> f a b) l2) l1


listAnyIJDiff : (a -> b -> Bool) -> List a -> List b -> Bool
listAnyIJDiff f l1 l2 =
    List.any (\a -> List.any (\b -> f a b) l2) l1


type MouseEventButton
    = LeftClick
    | MiddleClick
    | RightClick
    | BackClick
    | ForwardClick


buttonToMouseEventButton : Int -> MouseEventButton
buttonToMouseEventButton button =
    case button of
        0 ->
            LeftClick

        1 ->
            MiddleClick

        2 ->
            RightClick

        3 ->
            BackClick

        _ ->
            ForwardClick


type alias MouseEvent =
    { layerX : Int
    , layerY : Int
    , button : MouseEventButton
    }


type Resizable
    = Top
    | Bottom
    | Left
    | Right
