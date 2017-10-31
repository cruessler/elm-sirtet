module Tests exposing (..)

import Array exposing (Array)
import Board
    exposing
        ( Board
        , Piece
        , Position
        , Square(..)
        , emptyBoard
        , isLegalPosition
        , lockPiece
        )
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Game exposing (Game, Direction(..), isLegalMove)
import Random
import Test exposing (..)


piece : Piece
piece =
    [ [ Occupied, Occupied, Empty ]
    , [ Empty, Occupied, Occupied ]
    ]


position : Position
position =
    { x = 1, y = 0 }


width : Piece -> Int
width piece =
    case piece of
        [] ->
            0

        first :: rest ->
            List.length first


height : Piece -> Int
height piece =
    List.length piece


toBoard : Piece -> Board
toBoard piece =
    let
        pieces =
            piece
                |> List.map Array.fromList
                |> Array.fromList
    in
        { width = width piece, height = height piece, pieces = pieces }


occupiedBoard : Board
occupiedBoard =
    Board.initialize Board.rows Board.columns (\_ _ -> Occupied)


board : Test
board =
    describe "Board"
        [ describe "isLegalPosition"
            [ test "top left corner on an empty board" <|
                \_ ->
                    emptyBoard
                        |> isLegalPosition piece { x = 0, y = 0 }
                        |> Expect.equal True
            , test "outside an empty board" <|
                \_ ->
                    emptyBoard
                        |> isLegalPosition piece { x = -1, y = -1 }
                        |> Expect.equal False
            , test "top left corner on an occupied board" <|
                \_ ->
                    occupiedBoard
                        |> isLegalPosition piece { x = 0, y = 0 }
                        |> Expect.equal False
            ]
        , describe "lockPiece"
            [ fuzz2
                (intRange 0 (Board.columns - 3))
                (intRange 0 (Board.rows - 2))
                "locks piece at random position"
              <|
                \x y ->
                    let
                        board =
                            emptyBoard
                                |> lockPiece piece { x = x, y = y }
                    in
                        Expect.equal
                            (Board.slice x
                                y
                                (x + width piece)
                                (y + height piece)
                                board
                            )
                            (toBoard piece)
            ]
        ]


game : Test
game =
    describe "Game"
        [ describe "move"
            [ test "Left" <|
                \_ ->
                    Expect.equal { x = 0, y = 0 } (Game.move Left position)
            , test "Right" <|
                \_ ->
                    Expect.equal { x = 2, y = 0 } (Game.move Right position)
            , test "Down" <|
                \_ ->
                    Expect.equal { x = 1, y = 1 } (Game.move Down position)
            ]
        , describe "isLegalMove"
            [ fuzz int "move down on an empty board" <|
                \seed ->
                    Game.initialize (Random.initialSeed seed)
                        |> isLegalMove Down
                        |> Expect.equal True
            ]
        ]
