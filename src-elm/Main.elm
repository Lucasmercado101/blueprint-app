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
import Svg.Attributes as SA exposing (color, cx, cy, fill, fontSize, r, rx, ry, stroke, strokeWidth, version, viewBox, x, x1, x2, y, y1, y2)
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
        { editingRoom : Maybe RoomID
        , hoveringOverOrDraggingRoom : HoveringOverOrDraggingRoom
        }
    | Delete


type HoveringOverOrDraggingRoom
    = NotHoveringOverRoom
    | HoveringOverRoom RoomID
    | HoldingClickOnRoom RoomID
    | DraggingRoom
        { room : RoomID
        , mousePos : Point
        , initialMousePos : Point
        , isOverlappingAnother : Bool
        }


type DrawState
    = NotDrawing
    | SelectedStart
        { position : { start : Point, end : Point }
        , relativeStartingPoint : Point
        , isOverlappingAnotherRectangle : Bool
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
                        { editingRoom = Nothing
                        , hoveringOverOrDraggingRoom = NotHoveringOverRoom
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
                                            , isOverlappingAnotherRectangle = False
                                            , relativeStartingPoint = ( x, y )
                                            }
                                        )
                                , snappingPointsLine = Nothing
                              }
                            , Cmd.none
                            )

                        SelectedStart { position, isOverlappingAnotherRectangle } ->
                            if isOverlappingAnotherRectangle then
                                ( { model
                                    | mode = Draw NotDrawing
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

                Select { editingRoom } ->
                    let
                        onARoom : Maybe RoomID
                        onARoom =
                            model.rooms
                                |> getFirstRoom (\{ boundingBox } -> Rect.isPointOnRectangle (mouseDownRelCoords |> toGlobal model.mapPanOffset) boundingBox)
                                |> Maybe.map .id
                    in
                    case onARoom of
                        Just room ->
                            ( { model | mode = Select { editingRoom = editingRoom, hoveringOverOrDraggingRoom = HoldingClickOnRoom room } }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

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

                                isOverlappingAnotherRectangle : Bool
                                isOverlappingAnotherRectangle =
                                    let
                                        ( gx, gy ) =
                                            toGlobal position.start model.mapPanOffset

                                        ( w1, h1 ) =
                                            toGlobal position.end model.mapPanOffset |> (\( x1, y1 ) -> ( x1 - gx, y1 - gy ))
                                    in
                                    List.any
                                        (\rect ->
                                            rectanglesOverlap
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
                                                , isOverlappingAnotherRectangle = isOverlappingAnotherRectangle
                                            }
                                        )
                                , snappingPointsLine = bottomSideIsAlignedToAnotherRectangle
                              }
                            , Cmd.none
                            )

                Select { editingRoom, hoveringOverOrDraggingRoom } ->
                    let
                        roomImHoveringOver : Maybe RoomID
                        roomImHoveringOver =
                            model.rooms
                                |> getFirstRoom (\{ boundingBox } -> Rect.isPointOnRectangle (mouseMoveRelCoords |> toGlobal model.mapPanOffset) boundingBox)
                                |> Maybe.map .id
                    in
                    case hoveringOverOrDraggingRoom of
                        NotHoveringOverRoom ->
                            case roomImHoveringOver of
                                Just room ->
                                    ( { model | mode = Select { editingRoom = editingRoom, hoveringOverOrDraggingRoom = HoveringOverRoom room } }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        HoveringOverRoom _ ->
                            case roomImHoveringOver of
                                Just room ->
                                    ( { model | mode = Select { editingRoom = editingRoom, hoveringOverOrDraggingRoom = HoveringOverRoom room } }, Cmd.none )

                                Nothing ->
                                    ( { model | mode = Select { editingRoom = editingRoom, hoveringOverOrDraggingRoom = NotHoveringOverRoom } }, Cmd.none )

                        HoldingClickOnRoom room ->
                            ( { model
                                | mode =
                                    Select
                                        { editingRoom = editingRoom
                                        , hoveringOverOrDraggingRoom =
                                            DraggingRoom
                                                { room = room
                                                , mousePos = mouseMoveRelCoords |> toGlobal model.mapPanOffset
                                                , initialMousePos = mouseMoveRelCoords |> toGlobal model.mapPanOffset
                                                , isOverlappingAnother = False
                                                }
                                        }
                              }
                            , Cmd.none
                            )

                        DraggingRoom { room, initialMousePos } ->
                            ( { model
                                | mode =
                                    Select
                                        { editingRoom = editingRoom
                                        , hoveringOverOrDraggingRoom =
                                            DraggingRoom
                                                { room = room
                                                , initialMousePos = initialMousePos
                                                , mousePos = mouseMoveRelCoords |> toGlobal model.mapPanOffset

                                                -- TODO:
                                                , isOverlappingAnother = False
                                                }
                                        }
                              }
                            , Cmd.none
                            )

        MouseUp mouseUpRelCoords ->
            case model.mode of
                Pan ->
                    ( { model | holdingLeftMouseDown = False }, Cmd.none )

                Select { editingRoom, hoveringOverOrDraggingRoom } ->
                    case hoveringOverOrDraggingRoom of
                        NotHoveringOverRoom ->
                            ( model, Cmd.none )

                        HoveringOverRoom _ ->
                            ( model, Cmd.none )

                        HoldingClickOnRoom roomId ->
                            ( { model
                                | mode =
                                    Select
                                        { editingRoom =
                                            case editingRoom of
                                                Just roomImEditing ->
                                                    if roomImEditing == roomId then
                                                        Nothing

                                                    else
                                                        Just roomId

                                                Nothing ->
                                                    Just roomId
                                        , hoveringOverOrDraggingRoom = HoveringOverRoom roomId
                                        }
                              }
                            , Cmd.none
                            )

                        DraggingRoom { room } ->
                            -- TODO: confirm new position by changing rooms location
                            ( { model | mode = Select { editingRoom = editingRoom, hoveringOverOrDraggingRoom = HoveringOverRoom room } }, Cmd.none )

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
            ([ svg [ version "1.1", SA.width "1900", SA.height "800", viewBox "0 0 1900 800" ]
                ((case model.mode of
                    Delete ->
                        rect [] []

                    Pan ->
                        rect [] []

                    Select _ ->
                        rect [] []

                    Draw state ->
                        case state of
                            NotDrawing ->
                                rect [] []

                            SelectedStart { position, isOverlappingAnotherRectangle } ->
                                let
                                    { start, end } =
                                        position

                                    ( x1, y1 ) =
                                        start

                                    ( x2, y2 ) =
                                        end
                                in
                                rect
                                    [ x (x1 |> toString)
                                    , y (y1 |> toString)
                                    , SA.height ((y2 - y1) |> toString)
                                    , SA.width ((x2 - x1) |> toString)
                                    , strokeWidth "2"
                                    , stroke
                                        (if isOverlappingAnotherRectangle then
                                            "red"

                                         else
                                            "white"
                                        )
                                    , fill "transparent"
                                    ]
                                    []
                 )
                    :: backgroundGrid model.mapPanOffset
                    ++ (case model.mode of
                            Delete ->
                                model.rooms |> List.map (drawRectangle model.mapPanOffset False)

                            Pan ->
                                model.rooms |> List.map (drawRectangle model.mapPanOffset False)

                            Draw _ ->
                                model.rooms |> List.map (drawRectangle model.mapPanOffset False)

                            Select { editingRoom, hoveringOverOrDraggingRoom } ->
                                let
                                    -- TODO: Refactor
                                    roomBeingHoveredOverOrDragged : List RoomID
                                    roomBeingHoveredOverOrDragged =
                                        getIdOfRoomBeingHoveredOrDragged hoveringOverOrDraggingRoom
                                            |> Maybe.map List.singleton
                                            |> Maybe.withDefault []

                                    roomsBeingHoveredOver : List RoomID
                                    roomsBeingHoveredOver =
                                        case editingRoom of
                                            Just roomId ->
                                                roomId :: roomBeingHoveredOverOrDragged

                                            Nothing ->
                                                roomBeingHoveredOverOrDragged
                                in
                                (model.rooms
                                    |> (\r ->
                                            case hoveringOverOrDraggingRoom of
                                                DraggingRoom { room } ->
                                                    r |> List.filter (\{ id } -> id /= room)

                                                _ ->
                                                    r
                                       )
                                    |> List.map
                                        (\({ id } as room) ->
                                            drawRectangle model.mapPanOffset
                                                (List.any (\r -> r == id) roomsBeingHoveredOver)
                                                room
                                        )
                                )
                                    ++ [ case hoveringOverOrDraggingRoom of
                                            NotHoveringOverRoom ->
                                                S.text ""

                                            HoveringOverRoom _ ->
                                                S.text ""

                                            HoldingClickOnRoom _ ->
                                                S.text ""

                                            DraggingRoom { initialMousePos, mousePos, room } ->
                                                model.rooms
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
                                                            rect
                                                                [ x ((x1 - distFromSelectedX) - gx |> toString)
                                                                , y ((y1 - distFromSelectedY) - gy |> toString)
                                                                , SA.height (height |> toString)
                                                                , SA.width (width |> toString)
                                                                , strokeWidth "2"
                                                                , stroke "white"
                                                                , fill "rgba(255,255,255,0.1)"
                                                                ]
                                                                []
                                                        )
                                                    |> Maybe.withDefault (S.text "")
                                       ]
                       )
                    ++ (case model.snappingPointsLine of
                            Just ( firstP, secondP ) ->
                                drawSnappingLines model.mapPanOffset firstP secondP

                            Nothing ->
                                []
                       )
                 -- debug stuff
                 -- ++ (model.rooms |> List.map (Tuple.first >> drawRectanglePoint model.mapPanOffset))
                )
             , div [ style "color" "white" ] [ text ("Current View: " ++ (model.mapPanOffset |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (model.mapPanOffset |> (\( x, y ) -> y |> String.fromInt))) ]
             ]
                ++ (case model.mode of
                        Delete ->
                            []

                        Draw state ->
                            case state of
                                NotDrawing ->
                                    [ div [ style "color" "white" ] [ text "Current State: Not Drawing" ] ]

                                SelectedStart { position } ->
                                    let
                                        { start, end } =
                                            position
                                    in
                                    [ div [ style "color" "white" ] [ text "Current State: Selected Start" ]
                                    , div [ style "color" "white" ] [ text ("Current Start: " ++ (start |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (start |> (\( x, y ) -> y |> String.fromInt))) ]
                                    , div [ style "color" "white" ] [ text ("Current End: " ++ (end |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (end |> (\( x, y ) -> y |> String.fromInt))) ]
                                    ]

                        Pan ->
                            [ div [ style "color" "white" ] [ text ("Current Start (relative): " ++ (model.relativeView.start |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (model.relativeView.start |> (\( x, y ) -> y |> String.fromInt))) ] ]

                        Select _ ->
                            []
                   )
            )
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

                Select { editingRoom } ->
                    case editingRoom of
                        Nothing ->
                            []

                        Just roomId ->
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
            )
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


drawRectangle : Point -> Bool -> Room -> Svg Msg
drawRectangle globalViewPanOffset beingHoveredOver room =
    let
        ( gx, gy ) =
            globalViewPanOffset

        { x1, y1, width, height } =
            room.boundingBox

        { id } =
            room.name
    in
    S.g []
        [ rect
            [ x (x1 - gx |> toString)
            , y (y1 - gy |> toString)
            , SA.height (height |> toString)
            , SA.width (width |> toString)
            , strokeWidth "2"
            , stroke "white"
            , fill
                (if beingHoveredOver then
                    "rgba(255,255,255,0.1)"

                 else
                    "transparent"
                )
            ]
            []

        -- TODO: get bounding box to check for collisions and overflows
        -- and stuff when text gets too large
        , S.text_
            [ x
                ((x1
                    + width
                    // 2
                    - (case room.name.boundingBox of
                        Just bbox ->
                            bbox.width // 2

                        Nothing ->
                            0
                      )
                 )
                    - gx
                    |> String.fromInt
                )
            , y ((y1 + height // 2) - gy |> String.fromInt)
            , SA.class "svgText"
            , SA.fill "white"
            , SA.id (id |> UUID.toString)
            ]
            [ S.text room.name.value
            ]
        ]


drawRectanglePoint : Point -> Rectangle -> Svg Msg
drawRectanglePoint globalView { x1, y1, width, height } =
    let
        ( gx, gy ) =
            globalView
    in
    S.text_
        [ x (x1 - gx |> String.fromInt)
        , y (y1 - gy - 10 |> String.fromInt)
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


rectanglesOverlap : Rectangle -> Rectangle -> Bool
rectanglesOverlap firstRectangle secondRectangle =
    let
        x1 =
            firstRectangle.x1

        y1 =
            firstRectangle.y1

        width1 =
            firstRectangle.width

        height1 =
            firstRectangle.height

        x2 =
            secondRectangle.x1

        y2 =
            secondRectangle.y1

        width2 =
            secondRectangle.width

        height2 =
            secondRectangle.height
    in
    -- top left is between the start and end
    (x1 >= x2 && x1 <= x2 + width2 && y1 >= y2 && y1 <= y2 + height2)
        -- top right is between the start and end
        || (x1 + width1 >= x2 && x1 + width1 <= x2 + width2 && y1 >= y2 && y1 <= y2 + height2)
        -- bottom left is between the start and end
        || (x1 >= x2 && x1 <= x2 + width2 && y1 + height1 >= y2 && y1 + height1 <= y2 + height2)
        -- bottom right is between the start and end
        || (x1 + width1 >= x2 && x1 + width1 <= x2 + width2 && y1 + height1 >= y2 && y1 + height1 <= y2 + height2)
        -- same but with the other rectangle
        || (x2 >= x1 && x2 <= x1 + width1 && y2 >= y1 && y2 <= y1 + height1)
        || (x2 + width2 >= x1 && x2 + width2 <= x1 + width1 && y2 >= y1 && y2 <= y1 + height1)
        || (x2 >= x1 && x2 <= x1 + width1 && y2 + height2 >= y1 && y2 + height2 <= y1 + height1)
        || (x2 + width2 >= x1 && x2 + width2 <= x1 + width1 && y2 + height2 >= y1 && y2 + height2 <= y1 + height1)


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
