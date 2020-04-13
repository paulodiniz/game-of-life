module CanvasEvents exposing (Point, onClick)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json


type alias Point =
    { x : Float
    , y : Float
    }


type alias PositionData =
    { client : ( Float, Float )
    , offset : ( Float, Float )
    , body : ( Float, Float )
    , documentElement : ( Float, Float )
    }


positionInCanvas : PositionData -> Point
positionInCanvas { client, offset, body, documentElement } =
    let
        ( cx, cy ) =
            client

        ( ox, oy ) =
            offset

        ( bx, by ) =
            body

        ( dx, dy ) =
            documentElement
    in
    Point ((cx + bx + dx) - ox) ((cy + by + dy) - oy)


onClick : (Point -> msg) -> Attribute msg
onClick message =
    on "click" <|
        Json.map
            (positionInCanvas >> message)
            positionDecoder


positionDecoder : Json.Decoder PositionData
positionDecoder =
    Json.map4 PositionData
        (toTuple [ "clientX" ] [ "clientY" ])
        (toTuple [ "target", "offsetLeft" ] [ "target", "offsetTop" ])
        (toTuple [ "view", "document", "body", "scrollLeft" ] [ "view", "document", "body", "scrollTop" ])
        (toTuple [ "view", "document", "documentElement", "scrollLeft" ] [ "view", "document", "documentElement", "scrollTop" ])


toTuple : List String -> List String -> Json.Decoder ( Float, Float )
toTuple x y =
    Json.map2 Tuple.pair (Json.at x Json.float) (Json.at y Json.float)
