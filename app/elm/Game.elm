module Game exposing (Game(..), Direction(..), initialize, move, step)

import Array
import Board exposing (Board, Piece, Position)
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


randomPiece : Seed -> ( Maybe Piece, Seed )
randomPiece seed =
    let
        generator =
            Random.int 0 (Array.length Board.pieces - 1)
                |> Random.map
                    (\i -> Array.get i Board.pieces)
    in
        Random.step generator seed


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
            Board.emptyBoard

        ( piece, nextSeed ) =
            randomPiece seed
    in
        Running
            { seed = nextSeed
            , piece = piece |> Maybe.withDefault []
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
                            randomPiece game.seed

                        newBoard =
                            Board.lockPiece game.piece game.position game.board

                        newPosition =
                            initialPosition newBoard
                    in
                        Running
                            { seed = nextSeed
                            , piece = newPiece |> Maybe.withDefault []
                            , position = newPosition
                            , board = newBoard
                            }

        game ->
            game
