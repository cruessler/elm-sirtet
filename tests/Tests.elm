module Tests exposing (..)

import Array exposing (Array)
import Board
    exposing
        ( Board
        , Position
        , isLegalPosition
        , lockPiece
        )
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Game exposing (Game(..), Direction(..))
import Piece exposing (Piece, Square(..))
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


boardWithEmptyColumn : Int -> Int -> Board
boardWithEmptyColumn rows columns =
    Board.initialize
        rows
        columns
        (\x y ->
            if x == columns - 1 then
                Empty
            else
                Occupied
        )


board : Test
board =
    describe "Board"
        [ describe "isLegalPosition"
            [ test "top left corner on an empty board" <|
                \_ ->
                    Board.empty
                        |> isLegalPosition piece { x = 0, y = 0 }
                        |> Expect.equal True
            , test "outside an empty board" <|
                \_ ->
                    Board.empty
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
                            Board.empty
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
        , describe "compact"
            [ test "compacts occupied board" <|
                \_ ->
                    Expect.all
                        [ \( removedRows, _ ) -> Expect.equal 10 removedRows
                        , \( _, newBoard ) -> Expect.equal Board.empty newBoard
                        ]
                        (Board.compact occupiedBoard)
            , test "doesn’t remove rows in empty board" <|
                \_ ->
                    Expect.all
                        [ \( removedRows, _ ) -> Expect.equal 0 removedRows
                        , \( _, newBoard ) -> Expect.equal Board.empty newBoard
                        ]
                        (Board.compact Board.empty)
            , fuzz2
                (intRange 1 Board.rows)
                (intRange 1 Board.columns)
                "doesn’t remove rows in board with empty column"
              <|
                \rows columns ->
                    let
                        board =
                            boardWithEmptyColumn rows columns
                    in
                        board
                            |> Board.compact
                            |> Expect.all
                                [ \( removedRows, _ ) -> Expect.equal 0 removedRows
                                , \( _, newBoard ) -> Expect.equal board newBoard
                                ]
            ]
        ]


setBoard : Board -> Game -> Game
setBoard board game =
    case game of
        Running game ->
            Running { game | board = board }

        Lost game ->
            Lost { game | board = board }


setPosition : Position -> Game -> Game
setPosition position game =
    case game of
        Running game ->
            Running { game | position = position }

        _ ->
            game


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
        , describe "step"
            [ fuzz int "move down on an empty board" <|
                \seed ->
                    let
                        initialGame =
                            Game.initialize (Random.initialSeed seed)

                        game =
                            Game.step initialGame
                    in
                        case ( initialGame, game ) of
                            ( Running initialGame, Running game ) ->
                                Expect.notEqual initialGame.position game.position

                            _ ->
                                Expect.fail "expected both games to be running"
            , fuzz int "step locks piece and creates new piece if piece reaches end of board" <|
                \seed ->
                    let
                        initialGame =
                            Game.initialize (Random.initialSeed seed)

                        game =
                            case initialGame of
                                Running { piece, position, board } ->
                                    initialGame
                                        |> setPosition
                                            { position
                                                | y = board.height - height piece
                                            }
                                        |> Game.step

                                _ ->
                                    initialGame
                    in
                        case ( initialGame, game ) of
                            ( Running initialGame, Running game ) ->
                                Expect.all
                                    [ \game ->
                                        Expect.equal 0 game.position.y
                                    , \game ->
                                        Expect.equal
                                            (Board.slice initialGame.position.x
                                                (game.board.height - height initialGame.piece)
                                                (initialGame.position.x + width initialGame.piece)
                                                game.board.height
                                                game.board
                                            )
                                            (toBoard initialGame.piece)
                                    ]
                                    game

                            _ ->
                                Expect.fail "expected both games to be running"
            , test "loses when new piece cannot be placed on board" <|
                \_ ->
                    let
                        initialGame =
                            Game.initialize (Random.initialSeed 0)

                        game =
                            case initialGame of
                                Running { piece, position, board } ->
                                    initialGame
                                        |> setBoard occupiedBoard
                                        |> setPosition
                                            { position
                                                | y = board.height - height piece
                                            }
                                        |> Game.step

                                _ ->
                                    initialGame
                    in
                        case game of
                            Lost _ ->
                                Expect.pass

                            _ ->
                                Expect.fail "expected the game to be lost"
            ]
        ]
