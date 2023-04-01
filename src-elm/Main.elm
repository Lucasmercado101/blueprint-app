module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode as JD exposing (Decoder)
import Svg as S exposing (svg)
import Svg.Attributes as SvgA exposing (height, viewBox, width)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Model =
    {}



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



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
        [ style "background-color" background, style "width" "100vw", style "height" "100vh", on "click" (JD.succeed NoOp) ]
        [ svg [] []
        ]



-- OTHER


background : String
background =
    "#023770"
