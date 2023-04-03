module Main exposing (..)

import Browser
import Browser.Events exposing (onMouseMove)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Json.Decode as JD exposing (Decoder)
import Random
import Rect exposing (Point, Rectangle)
import Svg as S exposing (Svg, line, rect, svg)
import Svg.Attributes as SA exposing (color, cx, cy, fill, fontSize, r, rx, ry, stroke, strokeWidth, version, viewBox, x, x1, x2, y, y1, y2)
import UUID exposing (UUID)



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


type alias Model =
    { mapPanOffset : Point

    -- TODO: move this to Drag
    , relativeView :
        { start : Point
        , originalView : Point
        }
    , mode : Mode
    , holdingLeftMouseDown : Bool
    , rectangles : List ( Rectangle, UUID )
    , snappingPointsLine : Maybe ( ( Point, Point ), ( Point, Point ) )
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mapPanOffset = ( 0, 0 )
      , relativeView =
            { start = ( 0, 0 )
            , originalView = ( 0, 0 )
            }
      , mode = Drag
      , holdingLeftMouseDown = False
      , rectangles = []
      , snappingPointsLine = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Mode
    = Drag
    | Draw DrawState
    | Select SelectState


type SelectState
    = -- id if hovering over a rectangle
      NothingSelected (Maybe UUID)
    | RectangleSelected UUID


type DrawState
    = NotDrawing
    | SelectedStart
        { position : { start : Point, end : Point }
        , relativeStartingPoint : Point
        , isOverlappingAnotherRectangle : Bool
        }


type Msg
    = NoOp
    | Clicked
    | MouseMove Point
    | MouseDown Point
    | MouseUp
    | DrawMode
    | DragMode
    | SelectMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Clicked ->
            ( model, Cmd.none )

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

        DragMode ->
            ( { model
                | mode = Drag
                , relativeView =
                    { start = ( 0, 0 )
                    , originalView = ( 0, 0 )
                    }
              }
            , Cmd.none
            )

        SelectMode ->
            ( { model
                | mode = Select (NothingSelected Nothing)
                , relativeView =
                    { start = ( 0, 0 )
                    , originalView = ( 0, 0 )
                    }
              }
            , Cmd.none
            )

        MouseDown ( x, y ) ->
            case model.mode of
                Drag ->
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

                        SelectedStart { position } ->
                            let
                                { start, end } =
                                    position

                                ( x1, y1 ) =
                                    start

                                ( x2, y2 ) =
                                    end

                                ( ox, oy ) =
                                    model.mapPanOffset
                            in
                            ( { model
                                | rectangles =
                                    ( { x1 = x1 + ox
                                      , y1 = y1 + oy
                                      , width = x2 - x1
                                      , height = y2 - y1
                                      }
                                      --   TODO: change to use random, have it be a command
                                    , Random.step UUID.generator (Random.initialSeed (x1 + y1 + x2 + y2 + ox + oy + 12345))
                                        |> Tuple.first
                                    )
                                        :: model.rectangles
                                , mode = Draw NotDrawing
                                , snappingPointsLine = Nothing
                              }
                            , Cmd.none
                            )

                Select state ->
                    case state of
                        NothingSelected _ ->
                            Debug.todo "onClick select"

                        RectangleSelected _ ->
                            Debug.todo "onClick select"

        MouseMove ( x, y ) ->
            case model.mode of
                Drag ->
                    let
                        ( sx, sy ) =
                            model.relativeView.start

                        ( ox, oy ) =
                            model.relativeView.originalView

                        ( dx, dy ) =
                            ( sx - x, sy - y )
                    in
                    ( { model
                        | mapPanOffset = ( ox + dx, oy + dy )
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
                                        (List.map Tuple.first model.rectangles)

                                bottomSideIsAlignedToAnotherRectangle : Maybe ( ( Point, Point ), ( Point, Point ) )
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
                                    model.rectangles
                                        |> List.filter
                                            (\( { y1, height }, _ ) ->
                                                y1 + height <= bry + 10 && y1 + height >= bry - 10
                                            )
                                        |> (\l ->
                                                let
                                                    closestRectangleToTheRight : Maybe Rectangle
                                                    closestRectangleToTheRight =
                                                        List.foldl
                                                            (\next curr ->
                                                                let
                                                                    ( x1, _ ) =
                                                                        Rect.bottomLeft next
                                                                in
                                                                case curr of
                                                                    Just val ->
                                                                        if x1 >= brx && x1 <= (Rect.bottomLeft val |> Tuple.first) then
                                                                            Just next

                                                                        else
                                                                            curr

                                                                    Nothing ->
                                                                        if x1 >= brx then
                                                                            Just next

                                                                        else
                                                                            Nothing
                                                            )
                                                            Nothing
                                                            (l |> List.map Tuple.first)

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
                                                            (l |> List.map Tuple.first)

                                                    closestRectangle : Maybe Rectangle
                                                    closestRectangle =
                                                        case ( closestRectangleToTheRight, closestRectangleToTheLeft ) of
                                                            ( Just rr, Just rl ) ->
                                                                let
                                                                    x1 =
                                                                        rr |> Rect.bottomLeft |> Rect.x

                                                                    x2 =
                                                                        rl |> Rect.bottomRight |> Rect.x
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
                                                        rect |> Rect.bottomLeft |> Rect.x

                                                    br =
                                                        rect |> Rect.bottomRight |> Rect.x

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
                                                            ( ( currDrawRect |> Rect.bottomLeft |> Tuple.mapSecond (always (rect.y1 + rect.height))
                                                              , currDrawRect |> Rect.bottomRight |> Tuple.mapSecond (always (rect.y1 + rect.height))
                                                              )
                                                            , rect |> Rect.bottomSide
                                                            )

                                                    else
                                                        Nothing

                                                else if leftDist <= minDistBeforeSnapping then
                                                    Just
                                                        ( rect |> Rect.bottomSide
                                                        , ( currDrawRect |> Rect.bottomLeft |> Tuple.mapSecond (always (rect.y1 + rect.height))
                                                          , currDrawRect |> Rect.bottomRight |> Tuple.mapSecond (always (rect.y1 + rect.height))
                                                          )
                                                        )

                                                else
                                                    Nothing
                                            )

                                snapBottomPosition : Maybe { start : Point, end : Point }
                                snapBottomPosition =
                                    bottomSideIsAlignedToAnotherRectangle
                                        |> Maybe.map
                                            (\( ( ( _, y1 ), _ ), _ ) ->
                                                y1
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

                Select state ->
                    case state of
                        NothingSelected _ ->
                            ( { model
                                | mode =
                                    Select
                                        (NothingSelected
                                            (model.rectangles
                                                |> List.filter (\( rect, _ ) -> Rect.isOnRectangle ( x, y ) rect)
                                                |> List.head
                                                |> Maybe.map Tuple.second
                                            )
                                        )
                              }
                            , Cmd.none
                            )

                        RectangleSelected _ ->
                            Debug.todo ""

        MouseUp ->
            ( { model | holdingLeftMouseDown = False }, Cmd.none )


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
    let
        onDrag =
            [ onMouseUp MouseUp
            , if model.holdingLeftMouseDown then
                on "mousemove" (mouseMoveDecoder |> JD.map MouseMove)

              else
                on "mousemove" (JD.succeed NoOp)
            , if model.holdingLeftMouseDown then
                style "cursor" "grabbing"

              else
                style "cursor" "grab"
            , on "mousedown" (mouseMoveDecoder |> JD.map MouseDown)
            ]
    in
    div []
        [ div
            ([ style "background-color" background
             , style "width" "100vw"
             , style "height" "100vh"

             --  TODO: drag by holding middle click and moving mouse
             --  , on "auxclick"
             ]
                ++ (case model.mode of
                        Drag ->
                            onDrag

                        Draw state ->
                            [ onMouseUp MouseUp
                            , on "click" (mouseMoveDecoder |> JD.map MouseDown)
                            , case state of
                                NotDrawing ->
                                    style "" ""

                                SelectedStart _ ->
                                    on "mousemove" (mouseMoveDecoder |> JD.map MouseMove)
                            ]

                        Select state ->
                            [ on "mousemove" (mouseMoveDecoder |> JD.map MouseMove)
                            ]
                   )
            )
            -- Drawing order is bottom to top, draw last on top
            ([ svg [ version "1.1", SA.width "1900", SA.height "800", viewBox "0 0 1900 800" ]
                ((case model.mode of
                    Drag ->
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

                    Select state ->
                        case state of
                            NothingSelected _ ->
                                rect [] []

                            RectangleSelected _ ->
                                rect [] []
                 )
                    :: backgroundGrid model.mapPanOffset
                    ++ (case model.mode of
                            Drag ->
                                model.rectangles |> List.map (Tuple.first >> drawRectangle model.mapPanOffset False)

                            Draw _ ->
                                model.rectangles |> List.map (Tuple.first >> drawRectangle model.mapPanOffset False)

                            Select state ->
                                case state of
                                    NothingSelected hoveringOverRectangleId ->
                                        case hoveringOverRectangleId of
                                            Just hoveringId ->
                                                model.rectangles |> List.map (\( rect, id ) -> drawRectangle model.mapPanOffset (id == hoveringId) rect)

                                            Nothing ->
                                                model.rectangles |> List.map (Tuple.first >> drawRectangle model.mapPanOffset False)

                                    RectangleSelected _ ->
                                        model.rectangles |> List.map (Tuple.first >> drawRectangle model.mapPanOffset False)
                       )
                    ++ (case model.snappingPointsLine of
                            Just ( firstP, secondP ) ->
                                drawSnappingLines model.mapPanOffset firstP secondP

                            Nothing ->
                                []
                       )
                    -- debug stuff
                    ++ (model.rectangles |> List.map (Tuple.first >> drawRectanglePoint model.mapPanOffset))
                )
             , div [ style "color" "white" ] [ text ("Current View: " ++ (model.mapPanOffset |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (model.mapPanOffset |> (\( x, y ) -> y |> String.fromInt))) ]
             ]
                ++ (case model.mode of
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

                        Drag ->
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
            [ button [ style "padding" "5px", onClick DragMode ] [ text "Move" ]
            , button [ style "padding" "5px", onClick DrawMode ] [ text "Draw" ]
            , button [ style "padding" "5px", onClick SelectMode ] [ text "Select" ]
            ]
        ]


drawSnappingLines : Point -> ( Point, Point ) -> ( Point, Point ) -> List (Svg Msg)
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


drawRectangle : Point -> Bool -> Rectangle -> Svg Msg
drawRectangle globalViewPanOffset beingHoveredOver { x1, y1, width, height } =
    let
        ( gx, gy ) =
            globalViewPanOffset
    in
    rect
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


tupleSub : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
tupleSub ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


tupleAdd : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
tupleAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )
