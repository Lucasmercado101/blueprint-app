module Main exposing (..)

import Browser
import Browser.Events exposing (onMouseMove)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Json.Decode as JD exposing (Decoder)
import Svg as S exposing (Svg, rect, svg)
import Svg.Attributes as SvgA exposing (color, cx, cy, fill, fontSize, height, r, rx, ry, stroke, strokeWidth, version, viewBox, width, x, x1, x2, y, y1, y2)


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


type Shape
    = Rectangle
        { x1 : Int
        , y1 : Int
        , x2 : Int
        , y2 : Int
        }
    | Square
        { position : Point
        , size : Int
        }


type alias Model =
    { view : Point
    , relativeView :
        { start : Point
        , current : Point
        , originalView : Point
        }
    , mode : Mode
    , holdingLeftMouseDown : Bool
    , shapes : List Shape
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { view = ( 0, 0 )
      , relativeView =
            { start = ( 0, 0 )
            , current = ( 0, 0 )
            , originalView = ( 0, 0 )
            }
      , mode = Drag
      , holdingLeftMouseDown = False
      , shapes = []
      }
    , Cmd.none
    )



-- UPDATE


type Mode
    = Drag
    | Draw DrawState


type DrawState
    = NotDrawing
    | SelectedStart ( Point, Point, Point )


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
                    , current = ( 0, 0 )
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
                    , current = ( 0, 0 )
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
                            , current = ( 0, 0 )
                            , originalView = model.view
                            }
                      }
                    , Cmd.none
                    )

                Draw state ->
                    case state of
                        NotDrawing ->
                            ( { model
                                | mode = Draw (SelectedStart ( ( x, y ), ( x, y ), ( x, y ) ))
                              }
                            , Cmd.none
                            )

                        SelectedStart ( _, start, end ) ->
                            ( { model
                                | shapes =
                                    Rectangle
                                        { x1 = (start |> Tuple.first) - (model.view |> Tuple.first)
                                        , y1 = (start |> Tuple.second) - (model.view |> Tuple.second)
                                        , x2 = (end |> Tuple.first) - (model.view |> Tuple.first)
                                        , y2 = (end |> Tuple.second) - (model.view |> Tuple.second)
                                        }
                                        :: model.shapes
                                , mode = Draw NotDrawing
                              }
                            , Cmd.none
                            )

        MouseMove ( x, y ) ->
            case model.mode of
                Drag ->
                    let
                        relative =
                            model.relativeView

                        ( sx, sy ) =
                            relative.start

                        ( ox, oy ) =
                            relative.originalView

                        ( cx, cy ) =
                            ( (sx - x) * -1, (sy - y) * -1 )
                    in
                    ( { model
                        | view = ( ox + cx, oy + cy )
                        , relativeView =
                            { relative
                                | current = ( cx, cy )
                            }
                      }
                    , Cmd.none
                    )

                Draw state ->
                    case state of
                        NotDrawing ->
                            ( model, Cmd.none )

                        SelectedStart ( startingPoint, _, _ ) ->
                            let
                                ( xStart, yStart ) =
                                    startingPoint
                            in
                            ( { model
                                | mode =
                                    if x <= xStart && y <= yStart then
                                        Draw (SelectedStart ( startingPoint, ( x, y ), startingPoint ))

                                    else if y <= yStart then
                                        Draw (SelectedStart ( startingPoint, ( xStart, y ), ( x, yStart ) ))

                                    else if x <= xStart then
                                        Draw (SelectedStart ( startingPoint, ( x, yStart ), ( xStart, y ) ))

                                    else
                                        Draw (SelectedStart ( startingPoint, startingPoint, ( x, y ) ))
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
            model.view

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
            ([ svg [ version "1.1", width "1900", height "800", viewBox "0 0 1900 800" ]
                ((case model.mode of
                    Drag ->
                        rect [] []

                    Draw state ->
                        case state of
                            NotDrawing ->
                                rect [] []

                            SelectedStart ( _, start, end ) ->
                                let
                                    ( x1, y1 ) =
                                        start

                                    ( x2, y2 ) =
                                        end
                                in
                                rect
                                    [ x (x1 |> toString)
                                    , y (y1 |> toString)
                                    , height ((y2 - y1) |> toString)
                                    , width ((x2 - x1) |> toString)
                                    , strokeWidth "2"
                                    , stroke "white"
                                    , fill "transparent"
                                    ]
                                    []
                 )
                    :: (model.shapes |> List.map (drawShape model.view))
                    ++ backgroundGrid model.view
                    -- debug stuff
                    ++ (model.shapes |> List.map (drawShapePoint model.view))
                )
             , div [ style "color" "white" ] [ text ("Current View: " ++ (model.view |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (model.view |> (\( x, y ) -> y |> String.fromInt))) ]
             ]
                ++ (case model.mode of
                        Draw state ->
                            case state of
                                NotDrawing ->
                                    [ div [ style "color" "white" ] [ text "Current State: Not Drawing" ] ]

                                SelectedStart ( sp, start, end ) ->
                                    [ div [ style "color" "white" ] [ text "Current State: Selected Start" ]
                                    , div [ style "color" "white" ] [ text ("Current Start: " ++ (start |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (start |> (\( x, y ) -> y |> String.fromInt))) ]
                                    , div [ style "color" "white" ] [ text ("Current End: " ++ (end |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (end |> (\( x, y ) -> y |> String.fromInt))) ]
                                    , div [ style "color" "white" ] [ text ("Current StartingPoint: " ++ (sp |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (sp |> (\( x, y ) -> y |> String.fromInt))) ]
                                    ]

                        Drag ->
                            [ div [ style "color" "white" ] [ text ("Current Start: " ++ (model.relativeView.start |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (model.relativeView.start |> (\( x, y ) -> y |> String.fromInt))) ]
                            , div [ style "color" "white" ] [ text ("Current Relative to start: " ++ (model.relativeView.current |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (model.relativeView.current |> (\( x, y ) -> y |> String.fromInt))) ]
                            ]
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


drawShape : Point -> Shape -> Svg Msg
drawShape globalView shape =
    let
        ( gx, gy ) =
            globalView
    in
    case shape of
        Rectangle { x1, y1, x2, y2 } ->
            rect
                [ x ((x1 + gx) |> toString)
                , y ((y1 + gy) |> toString)
                , height ((y2 - y1) |> toString)
                , width ((x2 - x1) |> toString)
                , strokeWidth "2"
                , stroke "white"
                , fill "transparent"
                ]
                []

        Square { position, size } ->
            rect
                [ x (((position |> Tuple.first) + gx) |> toString)
                , y (((position |> Tuple.second) + gy) |> toString)
                , height (size |> toString)
                , width (size |> toString)
                , strokeWidth "2"
                , stroke "white"
                , fill "transparent"
                ]
                []


drawShapePoint : Point -> Shape -> Svg Msg
drawShapePoint globalView shape =
    let
        ( gx, gy ) =
            globalView
    in
    case shape of
        Rectangle { x1, y1, x2, y2 } ->
            S.text_
                [ x (x1 + gx |> String.fromInt)
                , y (y1 + gy - 10 |> String.fromInt)
                , SvgA.class "svgText"
                , SvgA.fill "white"
                ]
                [ S.text
                    ("X: "
                        ++ (x1 + gx |> String.fromInt)
                        ++ " Y: "
                        ++ (y1 + gy |> String.fromInt)
                        ++ " W: "
                        ++ (x2 - x1 |> String.fromInt)
                        ++ " H: "
                        ++ (y2 - y1 |> String.fromInt)
                    )
                ]

        Square { position, size } ->
            Debug.todo "square"



-- infinitely repeating grid


backgroundGrid : Point -> List (Svg Msg)
backgroundGrid ( gx, gy ) =
    let
        ( x, y ) =
            ( gx |> abs |> modBy 100, gy |> abs |> modBy 100 )
    in
    (List.range 0 40
        |> List.map
            (\i ->
                S.line
                    -- TODO: use dynamic screen resolution
                    [ x1 (i * 50 - x |> String.fromInt)
                    , y1 (0 - y |> String.fromInt)
                    , x2 (i * 50 - x |> String.fromInt)
                    , y2 (1200 - y |> String.fromInt)
                    , SvgA.stroke "white"
                    , SvgA.strokeWidth "1"
                    , SvgA.strokeDasharray "5,5"
                    ]
                    []
            )
    )
        ++ (List.range 0 18
                |> List.map
                    (\i ->
                        S.line
                            [ x1 (0 - x |> String.fromInt)
                            , y1 (i * 50 - y |> String.fromInt)
                            , x2 (1900 - x |> String.fromInt)
                            , y2 (i * 50 - y |> String.fromInt)
                            , SvgA.stroke "white"
                            , SvgA.strokeWidth "1"
                            , SvgA.strokeDasharray "5,5"
                            ]
                            []
                    )
           )



-- , S.g
--     [ SvgA.class "grid" ]
--     (List.range 0 7
--         |> List.map
--             (\i ->
--                 S.line
--                     [ x1 (0 - x |> String.fromInt)
--                     , y1 (i * 50 - y |> String.fromInt)
--                     , x2 (1900 - x |> String.fromInt)
--                     , y2 (i * 50 - y |> String.fromInt)
--                     , SvgA.stroke "white"
--                     , SvgA.strokeWidth "1"
--                     , SvgA.strokeDasharray "5,5"
--                     ]
--                     []
--             )
--     )
-- OTHER


background : String
background =
    "#023770"
