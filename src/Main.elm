module Main exposing (main)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes exposing (style)
import Time exposing (Posix)


type Msg
    = AnimationFrame Posix


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init () =
    (0, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        width =
            500

        height =
            300
    in
    Canvas.toHtml ( width, height )
        [ style "border" "1px solid black" ]
        [ shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]
        , renderSquare
        ]


renderSquare =
    shapes [ fill (Color.rgba 0 0 0 1) ]
        [ rect ( 0, 0 ) 100 50 ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
