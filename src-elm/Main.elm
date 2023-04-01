module Main exposing (..)

import Browser
import Browser.Events exposing (onMouseMove)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as JD exposing (Decoder)
import Svg as S exposing (rect, svg)
import Svg.Attributes as SvgA exposing (cx, cy, fill, fontSize, height, r, rx, ry, textAnchor, version, viewBox, width, x, y)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onMouseMove (mouseMoveDecoder |> JD.map MouseMove)


mouseMoveDecoder : Decoder ( Int, Int )
mouseMoveDecoder =
    JD.map2 (\x y -> ( x, y ))
        (JD.field "layerX" JD.int)
        (JD.field "layerY" JD.int)



-- MODEL


type alias Model =
    { view : ( Int, Int ) }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { view = ( 0, 0 )
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Clicked
    | MouseMove ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Clicked ->
            ( model, Cmd.none )

        MouseMove ( x, y ) ->
            ( { model | view = ( x, y ) }, Cmd.none )


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
        [ style "background-color" background, style "width" "100vw", style "height" "100vh" ]
        [ svg [ version "1.1", width "800", height "800", viewBox "0 0 800 800" ]
            [ rect [ width "50", height "50", fill "white", x (xPos |> toString), y (yPos |> toString) ] []
            ]
        ]



-- OTHER


background : String
background =
    "#023770"
