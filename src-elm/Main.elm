module Main exposing (..)

import Browser
import Browser.Events exposing (onMouseMove)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as JD exposing (Decoder)
import Svg as S exposing (rect, svg)
import Svg.Attributes as SvgA exposing (cx, cy, fill, fontSize, height, r, rx, ry, textAnchor, version, viewBox, width, x, y)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onMouseMove (JD.succeed NoOp)



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


update : msg -> model -> ( model, Cmd msg )
update _ model =
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
view _ =
    div
        [ style "background-color" background, style "width" "100vw", style "height" "100vh" ]
        [ svg [ version "1.1", width "300", height "300", viewBox "0 0 300 300" ]
            [ rect [ width "50", height "50", fill "white", x "10", y "10" ] []
            ]
        ]



-- OTHER


background : String
background =
    "#023770"
