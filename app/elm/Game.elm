module Game exposing
    ( Direction(..)
    , Game(..)
    , dropPiece
    , initialize
    , move
    , movePiece
    , pause
    , resume
    , step
    , turnPiece
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


type alias State =
    { seed : Seed
    , piece : Piece
    , nextPiece : Piece
    , position : Position
    , board : Board
    , round : Int
    , removedRows : Int
    , points : Int
    }


type Game
    = Running State
    | Paused State
    | Lost
        { seed : Seed
        , board : Board
        , round : Int
        , removedRows : Int
        , points : Int
        }


initialize : Int -> Int -> Seed -> Game
initialize rows columns seed =
    let
        board =
            Board.initialize rows columns (\_ _ -> Piece.Empty)

        ( piece, nextSeed ) =
            Piece.random seed

        ( nextPiece, nextSeed_ ) =
            Piece.random nextSeed
    in
    Running
        { seed = nextSeed_
        , piece = piece
        , nextPiece = nextPiece
        , position = initialPosition piece board
        , board = board
        , round = 1
        , removedRows = 0
        , points = 0
        }


initialPosition : Piece -> Board -> Position
initialPosition piece board =
    { x = (board.width - Piece.width piece) // 2, y = 0 }


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
        Running state ->
            let
                nextPosition =
                    move Down state.position
            in
            if Board.isLegalPosition state.piece nextPosition state.board then
                dropPiece <| Running { state | position = nextPosition }

            else
                Running state

        _ ->
            game


turnPiece : Piece.Direction -> Game -> Game
turnPiece direction game =
    case game of
        Running state ->
            let
                turnedPiece =
                    Piece.turn direction state.piece
            in
            if Board.isLegalPosition turnedPiece state.position state.board then
                Running { state | piece = turnedPiece }

            else
                Running state

        _ ->
            game


movePiece : Direction -> Game -> Game
movePiece direction game =
    case game of
        Running state ->
            let
                nextPosition =
                    move direction state.position
            in
            if Board.isLegalPosition state.piece nextPosition state.board then
                Running { state | position = nextPosition }

            else
                Running state

        _ ->
            game


pause : Game -> Game
pause game =
    case game of
        Running state ->
            Paused state

        _ ->
            game


resume : Game -> Game
resume game =
    case game of
        Paused state ->
            Running state

        _ ->
            game


step : Game -> Game
step game =
    case game of
        Running state ->
            let
                nextPosition =
                    move Down state.position
            in
            if Board.isLegalPosition state.piece nextPosition state.board then
                Running { state | position = nextPosition }

            else
                let
                    ( newPiece, nextSeed ) =
                        Piece.random state.seed

                    ( removedRows, newBoard ) =
                        state.board
                            |> Board.lockPiece state.piece state.position
                            |> Board.compact

                    newRound =
                        state.round + 1

                    newPoints =
                        state.points + points removedRows

                    newPosition =
                        initialPosition state.nextPiece newBoard
                in
                if Board.isLegalPosition newPiece newPosition newBoard then
                    Running
                        { seed = nextSeed
                        , piece = state.nextPiece
                        , nextPiece = newPiece
                        , position = newPosition
                        , board = newBoard
                        , round = newRound
                        , removedRows = state.removedRows + removedRows
                        , points = newPoints
                        }

                else
                    Lost
                        { seed = nextSeed
                        , board = newBoard
                        , round = state.round
                        , removedRows = state.removedRows + removedRows
                        , points = newPoints
                        }

        _ ->
            game
