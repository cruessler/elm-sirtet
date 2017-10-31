module Board
    exposing
        ( Board
        , Position
        , initialize
        , rows
        , columns
        , empty
        , isOccupied
        , isLegalPosition
        , slice
        , lockPiece
        , compact
        )

import Array exposing (Array)
import Piece exposing (Piece, Square(..))


rows : Int
rows =
    10


columns : Int
columns =
    8


type alias Board =
    { width : Int
    , height : Int
    , pieces : Array (Array Square)
    }


empty : Board
empty =
    initialize rows columns (\_ _ -> Empty)


initialize : Int -> Int -> (Int -> Int -> Square) -> Board
initialize rows columns f =
    { width = columns
    , height = rows
    , pieces =
        Array.initialize rows
            (\y ->
                Array.initialize columns
                    (\x -> f x y)
            )
    }


type alias Position =
    { x : Int, y : Int }


isOccupied : Int -> Int -> Board -> Maybe Bool
isOccupied x y board =
    board.pieces
        |> Array.get y
        |> Maybe.andThen (\row -> Array.get x row)
        |> Maybe.map (\square -> square == Occupied)


isLegalPosition : Piece -> Position -> Board -> Bool
isLegalPosition piece position board =
    List.indexedMap
        (\i row ->
            List.indexedMap
                (\j square ->
                    let
                        x =
                            position.x + j

                        y =
                            position.y + i
                    in
                        if square == Occupied then
                            not
                                (isOccupied x y board
                                    |> Maybe.withDefault True
                                )
                        else
                            True
                )
                row
        )
        piece
        |> List.concat
        |> List.all ((==) True)


slice : Int -> Int -> Int -> Int -> Board -> Board
slice x1 y1 x2 y2 board =
    let
        pieces =
            Array.slice y1 y2 board.pieces |> Array.map (Array.slice x1 x2)
    in
        { width = x2 - x1, height = y2 - y1, pieces = pieces }


lockPiece : Piece -> Position -> Board -> Board
lockPiece piece position board =
    let
        indexedPieces =
            piece
                |> List.indexedMap
                    (\i row ->
                        List.indexedMap
                            (\j square -> ( j, i, square ))
                            row
                    )
                |> List.concat

        newPieces =
            indexedPieces
                |> List.foldl
                    (\( x, y, square ) acc ->
                        case square of
                            Occupied ->
                                Array.set
                                    (position.y + y)
                                    (Array.get (position.y + y) acc
                                        |> Maybe.withDefault Array.empty
                                        |> Array.set (position.x + x) Occupied
                                    )
                                    acc

                            Empty ->
                                acc
                    )
                    board.pieces
    in
        { board | pieces = newPieces }


compact : Board -> ( Int, Board )
compact board =
    let
        newPieces =
            Array.filter
                (Array.toList >> List.all ((==) Occupied) >> not)
                board.pieces

        removedRows =
            board.height - Array.length newPieces

        replacementPieces =
            Array.repeat removedRows <|
                Array.repeat board.width Empty
    in
        ( removedRows
        , { board | pieces = Array.append replacementPieces newPieces }
        )
