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
    | Tick Time.Posix


type CellState
    = Dead
    | Alive


type alias Model =
    { rows : Int, cols : Int, cells : List (List CellState) }


aliveCells : Random.Generator (List ( Int, Int ))
aliveCells =
    let
        numberOfAlive =
            (numRows * numCols) * 10 // 100
    in
    Random.list numberOfAlive (Random.pair (Random.int 0 numRows) (Random.int 0 numCols))


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
    Time.every 1000 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SeedLife indexes ->
            ( { model | cells = seedLife model indexes }, Cmd.none )

        Tick time ->
            ( { model | cells = step model }, Cmd.none )


step : Model -> List (List CellState)
step model =
    let
        transformRow rowIndex column =
            List.indexedMap (\columnIndex value -> updateCell model rowIndex columnIndex value) column

        transformedCells =
            List.indexedMap transformRow model.cells
    in
    transformedCells


updateCell : Model -> Int -> Int -> CellState -> CellState
updateCell model x y state =
    let
        an =
            aliveNeighbours model ( x, y ) |> List.length
    in
    if an < 2 && state == Alive then
        Dead

    else if an == 2 && state == Alive then
        Alive

    else if an == 3 && state == Alive then
        Alive

    else if an > 3 && state == Alive then
        Dead

    else if an == 3 && state == Dead then
        Alive

    else
        state


getNeighbours : ( Int, Int ) -> List ( Int, Int )
getNeighbours ( x, y ) =
    let
        possible =
            [ ( x - 1, y - 1 )
            , ( x - 1, y )
            , ( x - 1, y + 1 )
            , ( x, y - 1 )
            , ( x, y + 1 )
            , ( x + 1, y - 1 )
            , ( x + 1, y )
            , ( x + 1, y + 1 )
            ]
    in
    possible
        |> List.filter (\point -> Tuple.first point >= 0 && Tuple.first point < numRows)
        |> List.filter (\point -> Tuple.second point >= 0 && Tuple.second point < numCols)


aliveNeighbours : Model -> ( Int, Int ) -> List ( Int, Int )
aliveNeighbours model point =
    let
        neighbours =
            getNeighbours point
    in
    List.filter (\neighbour -> isCellAlive model neighbour) neighbours


isCellAlive : Model -> ( Int, Int ) -> Bool
isCellAlive model point =
    let
        cells =
            model.cells

        x =
            Tuple.first point

        y =
            Tuple.second point

        column =
            cells |> Array.fromList |> Array.get x |> Maybe.withDefault []

        value =
            column |> Array.fromList |> Array.get y |> Maybe.withDefault Dead
    in
    case value of
        Dead ->
            False

        Alive ->
            True


seedLife : Model -> List ( Int, Int ) -> List (List CellState)
seedLife { cells } indexes =
    List.foldl seedLifeInCell cells indexes


seedLifeInCell : ( Int, Int ) -> List (List CellState) -> List (List CellState)
seedLifeInCell ( rowIndex, colIndex ) cells =
    let
        column =
            cells |> Array.fromList |> Array.get rowIndex |> Maybe.withDefault [] |> Array.fromList

        newColumn =
            Array.set colIndex Alive column |> Array.toList
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
    800


height =
    800


squareSize =
    10


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
