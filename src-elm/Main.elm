module Main exposing (..)

import Browser
import Browser.Events exposing (onMouseMove)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Json.Decode as JD exposing (Decoder)
import Svg as S exposing (Svg, rect, svg)
import Svg.Attributes as SA exposing (color, cx, cy, fill, fontSize, r, rx, ry, stroke, strokeWidth, version, viewBox, x, x1, x2, y, y1, y2)


type alias Point =
    ( Int, Int )



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


type alias Rectangle =
    { x1 : Int, y1 : Int, width : Int, height : Int }


type alias Model =
    { mapPanOffset : Point
    , relativeView :
        { start : Point
        , originalView : Point
        }
    , mode : Mode
    , holdingLeftMouseDown : Bool
    , rectangles : List Rectangle
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
      }
    , Cmd.none
    )



-- UPDATE


type Mode
    = Drag
    | Draw DrawState


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
                                    { x1 = x1 + ox
                                    , y1 = y1 + oy
                                    , width = x2 - x1
                                    , height = y2 - y1
                                    }
                                        :: model.rectangles
                                , mode = Draw NotDrawing
                              }
                            , Cmd.none
                            )

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
                                        (\{ x1, y1, width, height } ->
                                            -- top left is between the start and end
                                            (gx >= x1 && gx <= x1 + width && gy >= y1 && gy <= y1 + height)
                                                -- top right is between the start and end
                                                || (gx + w1 >= x1 && gx + w1 <= x1 + width && gy >= y1 && gy <= y1 + height)
                                                -- bottom left is between the start and end
                                                || (gx >= x1 && gx <= x1 + width && gy + h1 >= y1 && gy + h1 <= y1 + height)
                                                -- bottom right is between the start and end
                                                || (gx + w1 >= x1 && gx + w1 <= x1 + width && gy + h1 >= y1 && gy + h1 <= y1 + height)
                                                -- same but with the other rectangle
                                                || (x1 >= gx && x1 <= gx + w1 && y1 >= gy && y1 <= gy + h1)
                                                || (x1 + width >= gx && x1 + width <= gx + w1 && y1 >= gy && y1 <= gy + h1)
                                                || (x1 >= gx && x1 <= gx + w1 && y1 + height >= gy && y1 + height <= gy + h1)
                                                || (x1 + width >= gx && x1 + width <= gx + w1 && y1 + height >= gy && y1 + height <= gy + h1)
                                        )
                                        model.rectangles
                            in
                            ( { model
                                | mode =
                                    Draw
                                        (SelectedStart
                                            { selectedStart
                                                | position = position
                                                , isOverlappingAnotherRectangle = isOverlappingAnotherRectangle
                                            }
                                        )
                              }
                            , Cmd.none
                            )

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
        ( xPos, yPos ) =
            model.mapPanOffset

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
                   )
            )
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
                 )
                    :: (model.rectangles |> List.map (drawShape model.mapPanOffset))
                    ++ backgroundGrid model.mapPanOffset
                    -- debug stuff
                    ++ (model.rectangles |> List.map (drawShapePoint model.mapPanOffset))
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
            ]
        ]


drawShape : Point -> Rectangle -> Svg Msg
drawShape globalViewPanOffset { x1, y1, width, height } =
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
        , fill "transparent"
        ]
        []


drawShapePoint : Point -> Rectangle -> Svg Msg
drawShapePoint globalView { x1, y1, width, height } =
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
