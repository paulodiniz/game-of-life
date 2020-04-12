module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame, onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
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


centerX =
    width / 2


centerY =
    height / 2


view : Model -> Html Msg
view { count } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            [ clearScreen
            , render count
            ]
        ]


clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


render count =
    let
        size =
            width / 3

        x =
            -(size / 2)

        y =
            -(size / 2)
    in
    shapes
        [ transform
            [ translate centerX centerY
            , rotate (degrees (count * 3))
            ]
        , fill (Color.hsl (degrees (count / 4)) 0.3 0.7)
        ]
        [ rect ( x, y ) size size ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
