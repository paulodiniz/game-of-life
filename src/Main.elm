module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame, onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html, div)
import Time exposing (Posix)


type Msg
    = Frame Float


type alias Model =
    { count : Float }


init : () -> ( Model, Cmd Msg )
init () =
    ( { count = 0 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrameDelta Frame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( { model | count = model.count + 1 }, Cmd.none )


width =
    400


height =
    400

squareSize =
    50


view : Model -> Html Msg
view { count } =
    Canvas.toHtml ( width, height )
        []
        drawSquares


drawSquares =
    [ renderSquare 0 0
    , renderSquare 0 1
    , renderSquare 0 2
    , renderSquare 0 3
    , renderSquare 1 0
    , renderSquare 1 1
    , renderSquare 1 2
    , renderSquare 1 3
    , renderSquare 2 0
    , renderSquare 2 1
    , renderSquare 2 2
    , renderSquare 2 3
    ]


renderSquare line col =
    let
        posX =
            col * squareSize

        posY =
            line * squareSize
    in
    shapes [ fill (Color.rgba 0 0 0 1) ] [ rect ( posX, posY ) squareSize squareSize ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
