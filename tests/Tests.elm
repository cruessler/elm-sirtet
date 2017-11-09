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


nonSymmetricPiece : Piece
nonSymmetricPiece =
    [ [ Empty, Occupied, Empty ]
    , [ Occupied, Occupied, Occupied ]
    ]


piece : Test
piece =
    describe "Piece"
        [ describe "turn"
            [ test "turns simple piece clockwise" <|
                \_ ->
                    let
                        turnedPiece =
                            [ [ Occupied, Empty ]
                            , [ Occupied, Occupied ]
                            , [ Occupied, Empty ]
                            ]
                    in
                        Expect.equal turnedPiece (Piece.turn Piece.Clockwise nonSymmetricPiece)
            , test "turns simple piece counterclockwise" <|
                \_ ->
                    let
                        turnedPiece =
                            [ [ Empty, Occupied ]
                            , [ Occupied, Occupied ]
                            , [ Empty, Occupied ]
                            ]
                    in
                        Expect.equal turnedPiece (Piece.turn Piece.Counterclockwise nonSymmetricPiece)
            , test "turning clockwise and counterclockise is idempotent" <|
                \_ ->
                    Expect.equal
                        Piece.all
                        (Array.map
                            (Piece.turn Piece.Clockwise
                                >> Piece.turn Piece.Counterclockwise
                            )
                            Piece.all
                        )
            , test "turning clockwise 4 times is idempotent" <|
                \_ ->
                    let
                        f =
                            Piece.turn Piece.Clockwise
                    in
                        Expect.equal
                            Piece.all
                            (Array.map (f >> f >> f >> f) Piece.all)
            ]
        ]


position : Position
position =
    { x = 1, y = 0 }


toBoard : Piece -> Board
toBoard piece =
    let
        rows =
            piece
                |> List.map Array.fromList
                |> Array.fromList
    in
        { width = Piece.width piece, height = Piece.height piece, rows = rows }


occupiedBoard : Int -> Int -> Board
occupiedBoard rows columns =
    Board.initialize rows columns (\_ _ -> Occupied)


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


boardWithFullBottomRows : Int -> Int -> Int -> Board
boardWithFullBottomRows rows columns fullRows =
    Board.initialize
        rows
        columns
        (\x y ->
            if y < rows - fullRows then
                Empty
            else
                Occupied
        )


board : Test
board =
    describe "Board"
        [ test "isOccupied" <|
            \_ ->
                boardWithEmptyColumn Board.rows Board.columns
                    |> Board.isOccupied (Board.columns - 1) 0
                    |> Expect.equal (Just False)
        , describe "isLegalPosition"
            [ test "top left corner on an empty board" <|
                \_ ->
                    Board.empty
                        |> isLegalPosition nonSymmetricPiece { x = 0, y = 0 }
                        |> Expect.equal True
            , test "outside an empty board" <|
                \_ ->
                    Board.empty
                        |> isLegalPosition nonSymmetricPiece { x = -1, y = -1 }
                        |> Expect.equal False
            , test "top left corner on an occupied board" <|
                \_ ->
                    occupiedBoard Board.rows Board.columns
                        |> isLegalPosition nonSymmetricPiece { x = 0, y = 0 }
                        |> Expect.equal False
            , test "top right corner on a board with an empty column" <|
                \_ ->
                    boardWithEmptyColumn Board.rows Board.columns
                        |> isLegalPosition
                            [ [ Empty, Occupied ], [ Empty, Occupied ] ]
                            { x = Board.columns - 2, y = 0 }
                        |> Expect.equal True
            , fuzz2
                (intRange 2 Board.rows)
                (intRange 3 Board.columns)
                "with a single empty row and a 2-row piece"
              <|
                \rows columns ->
                    boardWithFullBottomRows rows columns (rows - 1)
                        |> isLegalPosition nonSymmetricPiece { x = (columns - 3) // 2, y = 0 }
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
                                |> lockPiece nonSymmetricPiece { x = x, y = y }
                    in
                        Expect.equal
                            (Board.slice x
                                y
                                (x + Piece.width nonSymmetricPiece)
                                (y + Piece.height nonSymmetricPiece)
                                board
                            )
                            (toBoard nonSymmetricPiece)
            ]
        , describe "compact"
            [ test "compacts occupied board" <|
                \_ ->
                    Expect.all
                        [ \( removedRows, _ ) -> Expect.equal Board.rows removedRows
                        , \( _, newBoard ) -> Expect.equal Board.empty newBoard
                        ]
                        (Board.compact <| occupiedBoard Board.rows Board.columns)
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

        Paused game ->
            Paused { game | board = board }

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
                            Game.initialize
                                Board.rows
                                Board.columns
                                (Random.initialSeed seed)

                        game =
                            Game.step initialGame
                    in
                        case ( initialGame, game ) of
                            ( Running initialGame, Running game ) ->
                                Expect.all
                                    [ .position >> Expect.notEqual initialGame.position
                                    , .round >> Expect.equal initialGame.round
                                    , .removedRows >> Expect.equal 0
                                    ]
                                    game

                            _ ->
                                Expect.fail "expected both games to be running"
            , fuzz int "step locks piece and creates new piece if piece reaches end of board" <|
                \seed ->
                    let
                        initialGame =
                            Game.initialize
                                Board.rows
                                Board.columns
                                (Random.initialSeed seed)

                        game =
                            case initialGame of
                                Running { piece, position, board } ->
                                    initialGame
                                        |> setPosition
                                            { position
                                                | y = board.height - Piece.height piece
                                            }
                                        |> Game.step

                                _ ->
                                    initialGame
                    in
                        case ( initialGame, game ) of
                            ( Running initialGame, Running game ) ->
                                Expect.all
                                    [ .position >> .y >> Expect.equal 0
                                    , .round >> Expect.equal 2
                                    , .points >> Expect.equal 0
                                    , .piece >> Expect.equal initialGame.nextPiece
                                    , .board
                                        >> (Board.slice initialGame.position.x
                                                (game.board.height - Piece.height initialGame.piece)
                                                (initialGame.position.x + Piece.width initialGame.piece)
                                                game.board.height
                                           )
                                        >> Expect.equal
                                            (toBoard initialGame.piece)
                                    ]
                                    game

                            _ ->
                                Expect.fail "expected both games to be running"
            , test "loses when new piece cannot be placed on board" <|
                \_ ->
                    let
                        initialGame =
                            Game.initialize
                                Board.rows
                                Board.columns
                                (Random.initialSeed 0)

                        game =
                            case initialGame of
                                Running { piece, position, board } ->
                                    initialGame
                                        |> setBoard (boardWithEmptyColumn Board.rows Board.columns)
                                        |> setPosition
                                            { position
                                                | y = board.height - Piece.height piece
                                            }
                                        |> Game.step

                                _ ->
                                    initialGame
                    in
                        case game of
                            Lost game ->
                                Expect.all
                                    [ always Expect.pass
                                    , .round >> Expect.equal 1
                                    ]
                                    game

                            _ ->
                                Expect.fail "expected the game to be lost"
            , test "gets points when full rows are compacted" <|
                \_ ->
                    let
                        initialGame =
                            Game.initialize
                                Board.rows
                                Board.columns
                                (Random.initialSeed 0)
                                |> setBoard (occupiedBoard 4 Board.columns)

                        game =
                            Game.step initialGame
                    in
                        case game of
                            Running game ->
                                Expect.all
                                    [ .round >> Expect.equal 2
                                    , .points >> Expect.equal 1000
                                    , .removedRows >> Expect.equal 4
                                    ]
                                    game

                            _ ->
                                Expect.fail "expected the game to be running"
            ]
        , describe "dropPiece"
            [ fuzz int "drops piece to the lowest legal position on the board" <|
                \seed ->
                    let
                        fullBottomRows =
                            2

                        board =
                            (boardWithFullBottomRows
                                Board.rows
                                Board.columns
                                fullBottomRows
                            )

                        initialGame =
                            Game.initialize
                                Board.rows
                                Board.columns
                                (Random.initialSeed 0)
                                |> setBoard board

                        game =
                            Game.dropPiece initialGame
                    in
                        case game of
                            Running game ->
                                Expect.equal
                                    (game.board.height - Piece.height game.piece - fullBottomRows)
                                    game.position.y

                            _ ->
                                Expect.fail "expected the game to be running"
            ]
        , describe "movePiece"
            [ test "moves piece on an empty board" <|
                \_ ->
                    let
                        initialGame =
                            Game.initialize
                                Board.rows
                                Board.columns
                                (Random.initialSeed 0)
                                |> setPosition { x = 0, y = 0 }

                        game =
                            initialGame
                                |> Game.movePiece Right
                                |> Game.movePiece Right
                                |> Game.movePiece Down
                                |> Game.movePiece Down
                                |> Game.movePiece Left
                    in
                        case game of
                            Running game ->
                                Expect.all
                                    [ .position >> .x >> Expect.equal 1
                                    , .position >> .y >> Expect.equal 2
                                    ]
                                    game

                            _ ->
                                Expect.fail "expected the game to be running"
            ]
        , describe "pause and resume"
            [ test "pauses and resumes game" <|
                \_ ->
                    let
                        initialGame =
                            Game.initialize
                                Board.rows
                                Board.columns
                                (Random.initialSeed 0)

                        pausedGame =
                            Game.pause initialGame

                        resumedGame =
                            Game.resume pausedGame
                    in
                        case ( initialGame, pausedGame, resumedGame ) of
                            ( Running _, Paused _, Running _ ) ->
                                Expect.pass

                            _ ->
                                Expect.fail "expected the game to be paused and resumed"
            ]
        ]
