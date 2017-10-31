module Game exposing (Game, Direction(..), initialize, move, isLegalMove)

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


type alias Game =
    { seed : Seed
    , piece : Piece
    , position : Position
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
        { seed = nextSeed
        , piece = piece |> Maybe.withDefault []
        , position = initialPosition board
        , board = board
        }


initialPosition : Board -> Position
initialPosition board =
    { x = board.width // 2 - 2, y = 0 }


isLegalMove : Direction -> Game -> Bool
isLegalMove direction game =
    Board.isLegalPosition
        game.piece
        (move direction game.position)
        game.board
