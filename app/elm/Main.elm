module Main exposing (main)

import Array exposing (Array)
import Board exposing (Board, Position)
import Dict exposing (Dict)
import Game exposing (Game(..))
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Piece exposing (Piece, Square(..))
import Random
import Task
import Time exposing (Time)


type alias Model =
    Maybe Game


type Msg
    = StartGame
    | NewGame Time


main =
    H.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.none )


columns : Int
columns =
    12


rows : Int
rows =
    14


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( model, Task.perform NewGame Time.now )

        NewGame now ->
            let
                game =
                    Random.initialSeed (round now)
                        |> Game.initialize rows columns
            in
                ( Just game, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


squaresForBoard : Board -> Dict ( Int, Int ) (List ( String, Bool ))
squaresForBoard board =
    board
        |> Board.indexedMap (\x y square -> ( x, y, square ))
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat
        |> List.foldl
            (\( x, y, square ) acc ->
                case (square) of
                    Occupied ->
                        Dict.insert ( y, x ) [ ( "occupied", True ) ] acc

                    Empty ->
                        Dict.insert ( y, x ) [] acc
            )
            Dict.empty


squaresForPiece : Position -> Piece -> Dict ( Int, Int ) (List ( String, Bool ))
squaresForPiece position piece =
    piece
        |> Piece.indexedMap (\x y square -> ( x, y, square ))
        |> List.concat
        |> List.foldl
            (\( x, y, square ) acc ->
                case square of
                    Occupied ->
                        Dict.insert
                            ( position.y + y, position.x + x )
                            [ ( "piece", True ), ( "occupied", True ) ]
                            acc

                    Empty ->
                        acc
            )
            Dict.empty


board : Position -> Piece -> Board -> List (Html Msg)
board position piece board =
    squaresForBoard board
        |> Dict.union (squaresForPiece position piece)
        |> Dict.map
            (\_ classList ->
                H.div [ (( "square", True ) :: classList) |> A.classList ] []
            )
        |> Dict.values


lostBoard : Board -> List (Html Msg)
lostBoard board =
    squaresForBoard board
        |> Dict.map
            (\_ classList ->
                H.div [ (( "square", True ) :: classList) |> A.classList ] []
            )
        |> Dict.values


content : List (Html Msg) -> Html Msg
content =
    H.main_
        [ A.style
            [ ( "width", (toString <| columns * 3) ++ "em" )
            , ( "height", (toString <| rows * 3) ++ "em" )
            ]
        ]


grid : List (Html Msg) -> Html Msg
grid =
    H.div
        [ A.class "board"
        , A.style
            [ ( "grid-template-columns", "repeat(" ++ (toString columns) ++ ", 1fr)" )
            , ( "grid-template-rows", "repeat(" ++ (toString rows) ++ ", 1fr)" )
            ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Just (Running game) ->
            content
                [ (board game.position game.piece game.board)
                    |> grid
                ]

        Just (Lost game) ->
            content
                [ (lostBoard game.board)
                    |> grid
                ]

        _ ->
            content
                [ H.button
                    [ E.onClick StartGame ]
                    [ H.text "Start new game" ]
                ]
