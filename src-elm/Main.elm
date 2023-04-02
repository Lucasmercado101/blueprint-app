module Main exposing (..)

import Browser
import Browser.Events exposing (onMouseMove)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Json.Decode as JD exposing (Decoder)
import Svg as S exposing (rect, svg)
import Svg.Attributes as SvgA exposing (cx, cy, fill, fontSize, height, r, rx, ry, textAnchor, version, viewBox, width, x, y)



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
-- type alias Vec2 =
--     ( Int, Int )


type alias Model =
    { view : ( Int, Int )
    , relativeView :
        { start : ( Int, Int )
        , current : ( Int, Int )
        , originalView : ( Int, Int )
        }
    , holdingLeftMouseDown : Bool
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
      , holdingLeftMouseDown = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Clicked
    | MouseMove ( Int, Int )
    | MouseDown ( Int, Int )
    | MouseUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Clicked ->
            ( model, Cmd.none )

        MouseDown ( x, y ) ->
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

        MouseMove ( x, y ) ->
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
    in
    div
        [ style "background-color" background
        , style "width" "100vw"
        , style "height" "100vh"
        , onMouseUp MouseUp
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
        [ svg [ version "1.1", width "800", height "800", viewBox "0 0 800 800" ]
            [ rect [ width "50", height "50", fill "white", x (xPos |> toString), y (yPos |> toString) ] []
            ]
        , div [ style "color" "white" ] [ text ("Current View: " ++ (model.view |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (model.view |> (\( x, y ) -> y |> String.fromInt))) ]
        , div [ style "color" "white" ] [ text ("Current Start: " ++ (model.relativeView.start |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (model.relativeView.start |> (\( x, y ) -> y |> String.fromInt))) ]
        , div [ style "color" "white" ] [ text ("Current Relative to start: " ++ (model.relativeView.current |> (\( x, y ) -> x |> String.fromInt)) ++ ", " ++ (model.relativeView.current |> (\( x, y ) -> y |> String.fromInt))) ]
        ]



-- OTHER


background : String
background =
    "#023770"
