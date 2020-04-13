module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrame, onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color exposing (Color)
import Html exposing (Html, div)
import Random
import Time exposing (Posix)


type Msg
    = SeedLife (List ( Int, Int ))
    | Frame Float


type CellState
    = Dead
    | Alive


type alias Model =
    { rows : Int, cols : Int, cells : List (List CellState) }


aliveCells : Random.Generator (List ( Int, Int ))
aliveCells =
    Random.list 10 (Random.pair (Random.int 0 numRows) (Random.int 0 numCols))


init : () -> ( Model, Cmd Msg )
init () =
    ( { rows = numRows, cols = numCols, cells = initialCells }, Random.generate SeedLife aliveCells )


initialCells =
    let
        initCell index =
            Dead

        initializeCols _ =
            Array.initialize numCols initCell |> Array.toList
    in
    Array.initialize numRows initializeCols |> Array.toList


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrameDelta Frame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( model, Cmd.none )

        SeedLife indexes ->
            let
                _ =
                    Debug.log "Update sending" indexes
            in
            ( { model | cells = seedLife model indexes }, Cmd.none )


seedLife : Model -> List ( Int, Int ) -> List (List CellState)
seedLife { cells } indexes =
    List.foldl seedLifeInCell cells indexes


seedLifeInCell : ( Int, Int ) -> List (List CellState) -> List (List CellState)
seedLifeInCell ( rowIndex, colIndex ) cells =
    let
        column =
            cells |> Array.fromList |> Array.get rowIndex |> Maybe.withDefault([]) |> Array.fromList

        newColumn
            = Array.set colIndex Alive column |> Array.toList
    in
    Array.set rowIndex newColumn (cells |> Array.fromList) |> Array.toList


numRows =
    round <| height / squareSize


numCols =
    round <| width / squareSize


view : Model -> Html Msg
view model =
    Canvas.toHtml ( width, height )
        []
        (drawSquares model)


width =
    400


height =
    400


squareSize =
    50


drawSquares : Model -> List Renderable
drawSquares { rows, cols, cells } =
    List.indexedMap parseRow cells |> List.concat


parseRow : Int -> List CellState -> List Renderable
parseRow rowIndex rowCells =
    List.indexedMap (\columnIndex cell -> renderCell rowIndex columnIndex cell) rowCells


renderCell : Int -> Int -> CellState -> Renderable
renderCell rowIndex columnIndex cell =
    let
        color =
            case cell of
                Alive ->
                    Color.rgb 0 0 0

                Dead ->
                    Color.rgb 1 1 1
    in
    renderSquare rowIndex columnIndex color


renderSquare : Int -> Int -> Color -> Renderable
renderSquare line col color =
    let
        posX =
            col * squareSize |> toFloat

        posY =
            line * squareSize |> toFloat
    in
    shapes [ fill color ] [ rect ( posX, posY ) squareSize squareSize ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
