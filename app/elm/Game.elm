module Game exposing (Game(..), Direction(..), initialize, move, step)

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
        }
    | Lost
        { seed : Seed
        , board : Board
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
            }


initialPosition : Board -> Position
initialPosition board =
    { x = board.width // 2 - 2, y = 0 }


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

                        newBoard =
                            Board.lockPiece game.piece game.position game.board

                        newPosition =
                            initialPosition newBoard
                    in
                        if Board.isLegalPosition newPiece newPosition newBoard then
                            Running
                                { seed = nextSeed
                                , piece = newPiece
                                , position = newPosition
                                , board = newBoard
                                }
                        else
                            Lost { seed = nextSeed, board = newBoard }

        game ->
            game
