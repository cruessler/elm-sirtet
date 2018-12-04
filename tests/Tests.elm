module Tests exposing (testBoard, testGame, testPiece)

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
import Game exposing (Direction(..), Game(..))
import Piece exposing (Piece, Square(..))
import Random
import Test exposing (..)


nonSymmetricPiece : Piece
nonSymmetricPiece =
    [ [ Empty, Occupied, Empty ]
    , [ Occupied, Occupied, Occupied ]
    ]


testPiece : Test
testPiece =
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


defaultPosition : Position
defaultPosition =
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


testBoard : Test
testBoard =
    describe "Board"
        [ test "isOccupied" <|
            \_ ->
                boardWithEmptyColumn Board.defaultRows Board.defaultColumns
                    |> Board.isOccupied (Board.defaultColumns - 1) 0
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
                    occupiedBoard Board.defaultRows Board.defaultColumns
                        |> isLegalPosition nonSymmetricPiece { x = 0, y = 0 }
                        |> Expect.equal False
            , test "top right corner on a board with an empty column" <|
                \_ ->
                    boardWithEmptyColumn Board.defaultRows Board.defaultColumns
                        |> isLegalPosition
                            [ [ Empty, Occupied ], [ Empty, Occupied ] ]
                            { x = Board.defaultColumns - 2, y = 0 }
                        |> Expect.equal True
            , fuzz2
                (intRange 2 Board.defaultRows)
                (intRange 3 Board.defaultColumns)
                "with a single empty row and a 2-row piece"
              <|
                \rows columns ->
                    boardWithFullBottomRows rows columns (rows - 1)
                        |> isLegalPosition nonSymmetricPiece { x = (columns - 3) // 2, y = 0 }
                        |> Expect.equal False
            ]
        , describe "lockPiece"
            [ fuzz2
                (intRange 0 (Board.defaultColumns - 3))
                (intRange 0 (Board.defaultRows - 2))
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
                        [ \( removedRows, _ ) -> Expect.equal Board.defaultRows removedRows
                        , \( _, newBoard ) -> Expect.equal Board.empty newBoard
                        ]
                        (Board.compact <| occupiedBoard Board.defaultRows Board.defaultColumns)
            , test "doesn’t remove rows in empty board" <|
                \_ ->
                    Expect.all
                        [ \( removedRows, _ ) -> Expect.equal 0 removedRows
                        , \( _, newBoard ) -> Expect.equal Board.empty newBoard
                        ]
                        (Board.compact Board.empty)
            , fuzz2
                (intRange 1 Board.defaultRows)
                (intRange 1 Board.defaultColumns)
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
        Running state ->
            Running { state | board = board }

        Paused state ->
            Paused { state | board = board }

        Lost state ->
            Lost { state | board = board }


setPosition : Position -> Game -> Game
setPosition position game =
    case game of
        Running state ->
            Running { state | position = position }

        _ ->
            game


testGame : Test
testGame =
    describe "Game"
        [ describe "move"
            [ test "Left" <|
                \_ ->
                    Expect.equal { x = 0, y = 0 } (Game.move Left defaultPosition)
            , test "Right" <|
                \_ ->
                    Expect.equal { x = 2, y = 0 } (Game.move Right defaultPosition)
            , test "Down" <|
                \_ ->
                    Expect.equal { x = 1, y = 1 } (Game.move Down defaultPosition)
            ]
        , describe "step"
            [ fuzz int "move down on an empty board" <|
                \seed ->
                    let
                        initialGame =
                            Game.initialize
                                Board.defaultRows
                                Board.defaultColumns
                                (Random.initialSeed seed)

                        game =
                            Game.step initialGame
                    in
                        case ( initialGame, game ) of
                            ( Running initialState, Running state ) ->
                                Expect.all
                                    [ .position >> Expect.notEqual initialState.position
                                    , .round >> Expect.equal initialState.round
                                    , .removedRows >> Expect.equal 0
                                    ]
                                    state

                            _ ->
                                Expect.fail "expected both games to be running"
            , fuzz int "step locks piece and creates new piece if piece reaches end of board" <|
                \seed ->
                    let
                        initialGame =
                            Game.initialize
                                Board.defaultRows
                                Board.defaultColumns
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
                            ( Running initialState, Running state ) ->
                                Expect.all
                                    [ .position >> .y >> Expect.equal 0
                                    , .round >> Expect.equal 2
                                    , .points >> Expect.equal 0
                                    , .piece >> Expect.equal initialState.nextPiece
                                    , .board
                                        >> Board.slice initialState.position.x
                                            (state.board.height - Piece.height initialState.piece)
                                            (initialState.position.x + Piece.width initialState.piece)
                                            state.board.height
                                        >> Expect.equal
                                            (toBoard initialState.piece)
                                    ]
                                    state

                            _ ->
                                Expect.fail "expected both games to be running"
            , test "loses when new piece cannot be placed on board" <|
                \_ ->
                    let
                        initialGame =
                            Game.initialize
                                Board.defaultRows
                                Board.defaultColumns
                                (Random.initialSeed 0)

                        game =
                            case initialGame of
                                Running { piece, position, board } ->
                                    initialGame
                                        |> setBoard (boardWithEmptyColumn Board.defaultRows Board.defaultColumns)
                                        |> setPosition
                                            { position
                                                | y = board.height - Piece.height piece
                                            }
                                        |> Game.step

                                _ ->
                                    initialGame
                    in
                        case game of
                            Lost state ->
                                Expect.all
                                    [ always Expect.pass
                                    , .round >> Expect.equal 1
                                    ]
                                    state

                            _ ->
                                Expect.fail "expected the game to be lost"
            , test "gets points when full rows are compacted" <|
                \_ ->
                    let
                        initialGame =
                            Game.initialize
                                Board.defaultRows
                                Board.defaultColumns
                                (Random.initialSeed 0)
                                |> setBoard (occupiedBoard 4 Board.defaultColumns)

                        game =
                            Game.step initialGame
                    in
                        case game of
                            Running state ->
                                Expect.all
                                    [ .round >> Expect.equal 2
                                    , .points >> Expect.equal 1000
                                    , .removedRows >> Expect.equal 4
                                    ]
                                    state

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
                            boardWithFullBottomRows
                                Board.defaultRows
                                Board.defaultColumns
                                fullBottomRows

                        initialGame =
                            Game.initialize
                                Board.defaultRows
                                Board.defaultColumns
                                (Random.initialSeed 0)
                                |> setBoard board

                        game =
                            Game.dropPiece initialGame
                    in
                        case game of
                            Running state ->
                                Expect.equal
                                    (state.board.height - Piece.height state.piece - fullBottomRows)
                                    state.position.y

                            _ ->
                                Expect.fail "expected the game to be running"
            ]
        , describe "movePiece"
            [ test "moves piece on an empty board" <|
                \_ ->
                    let
                        initialGame =
                            Game.initialize
                                Board.defaultRows
                                Board.defaultColumns
                                (Random.initialSeed 0)
                                |> setPosition { x = 0, y = 0 }

                        gameAfterMoves =
                            initialGame
                                |> Game.movePiece Right
                                |> Game.movePiece Right
                                |> Game.movePiece Down
                                |> Game.movePiece Down
                                |> Game.movePiece Left
                    in
                        case gameAfterMoves of
                            Running state ->
                                Expect.all
                                    [ .position >> .x >> Expect.equal 1
                                    , .position >> .y >> Expect.equal 2
                                    ]
                                    state

                            _ ->
                                Expect.fail "expected the game to be running"
            ]
        , describe "pause and resume"
            [ test "pauses and resumes game" <|
                \_ ->
                    let
                        initialGame =
                            Game.initialize
                                Board.defaultRows
                                Board.defaultColumns
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
