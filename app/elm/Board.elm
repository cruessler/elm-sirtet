module Board
    exposing
        ( Board
        , Position
        , rows
        , columns
        , empty
        , initialize
        , indexedMap
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


get : Int -> Int -> Board -> Maybe Square
get x y board =
    board.pieces
        |> Array.get y
        |> Maybe.andThen (\row -> Array.get x row)


indexedMap : (Int -> Int -> Square -> a) -> Board -> Array (Array a)
indexedMap f board =
    Array.indexedMap
        (\y row ->
            (Array.indexedMap
                (\x square -> f x y square)
                row
            )
        )
        board.pieces


isOccupied : Int -> Int -> Board -> Maybe Bool
isOccupied x y =
    get x y >> Maybe.map ((==) Occupied)


isLegalPosition : Piece -> Position -> Board -> Bool
isLegalPosition piece position board =
    piece
        |> Piece.indexedMap
            (\x y square ->
                if square == Occupied then
                    isOccupied (position.x + x) (position.y + y) board
                        |> Maybe.map not
                        |> Maybe.withDefault False
                else
                    True
            )
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
                |> Piece.indexedMap (\x y square -> ( x, y, square ))
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
