module Game
    exposing
        ( Game(..)
        , Direction(..)
        , initialize
        , move
        , dropPiece
        , turnPiece
        , movePiece
        , step
        )

import Array
import Board exposing (Board, Position)
import Piece exposing (Piece)
import Random exposing (Seed)


type Direction
    = Left
    | Right
    | Down


move : Direction -> Position -> Position
move direction position =
    case direction of
        Left ->
            { position | x = position.x - 1 }

        Right ->
            { position | x = position.x + 1 }

        Down ->
            { position | y = position.y + 1 }


type Game
    = Running
        { seed : Seed
        , piece : Piece
        , position : Position
        , board : Board
        , points : Int
        }
    | Lost
        { seed : Seed
        , board : Board
        , points : Int
        }


initialize : Seed -> Game
initialize seed =
    let
        board =
            Board.empty

        ( piece, nextSeed ) =
            Piece.random seed
    in
        Running
            { seed = nextSeed
            , piece = piece
            , position = initialPosition board
            , board = board
            , points = 0
            }


initialPosition : Board -> Position
initialPosition board =
    { x = board.width // 2 - 2, y = 0 }


points : Int -> Int
points removedRows =
    case removedRows of
        1 ->
            100

        2 ->
            300

        3 ->
            600

        4 ->
            1000

        _ ->
            0


dropPiece : Game -> Game
dropPiece game =
    case game of
        Running game ->
            let
                nextPosition =
                    move Down game.position
            in
                if Board.isLegalPosition game.piece nextPosition game.board then
                    dropPiece <| Running { game | position = nextPosition }
                else
                    Running game

        game ->
            game


turnPiece : Piece.Direction -> Game -> Game
turnPiece direction game =
    case game of
        Running game ->
            let
                turnedPiece =
                    Piece.turn direction game.piece
            in
                if Board.isLegalPosition turnedPiece game.position game.board then
                    Running { game | piece = turnedPiece }
                else
                    Running game

        game ->
            game


movePiece : Direction -> Game -> Game
movePiece direction game =
    case game of
        Running game ->
            let
                nextPosition =
                    move direction game.position
            in
                if Board.isLegalPosition game.piece nextPosition game.board then
                    Running { game | position = nextPosition }
                else
                    Running game

        game ->
            game


step : Game -> Game
step game =
    case game of
        Running game ->
            let
                nextPosition =
                    move Down game.position
            in
                if Board.isLegalPosition game.piece nextPosition game.board then
                    Running { game | position = nextPosition }
                else
                    let
                        ( newPiece, nextSeed ) =
                            Piece.random game.seed

                        ( removedRows, newBoard ) =
                            game.board
                                |> Board.lockPiece game.piece game.position
                                |> Board.compact

                        newPoints =
                            game.points + points removedRows

                        newPosition =
                            initialPosition newBoard
                    in
                        if Board.isLegalPosition newPiece newPosition newBoard then
                            Running
                                { seed = nextSeed
                                , piece = newPiece
                                , position = newPosition
                                , board = newBoard
                                , points = newPoints
                                }
                        else
                            Lost
                                { seed = nextSeed
                                , board = newBoard
                                , points = newPoints
                                }

        game ->
            game
