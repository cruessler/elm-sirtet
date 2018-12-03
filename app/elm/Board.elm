module Board exposing
    ( Board
    , Position
    , compact
    , defaultColumns
    , defaultRows
    , empty
    , indexedMap
    , initialize
    , isLegalPosition
    , isOccupied
    , lockPiece
    , slice
    )

import Array exposing (Array)
import Piece exposing (Piece, Square(..))


defaultRows : Int
defaultRows =
    10


defaultColumns : Int
defaultColumns =
    8


type alias Board =
    { width : Int
    , height : Int
    , rows : Array (Array Square)
    }


empty : Board
empty =
    initialize defaultRows defaultColumns (\_ _ -> Empty)


initialize : Int -> Int -> (Int -> Int -> Square) -> Board
initialize rows columns f =
    { width = columns
    , height = rows
    , rows =
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
    board.rows
        |> Array.get y
        |> Maybe.andThen (\row -> Array.get x row)


indexedMap : (Int -> Int -> Square -> a) -> Board -> Array (Array a)
indexedMap f board =
    Array.indexedMap
        (\y row ->
            Array.indexedMap
                (\x square -> f x y square)
                row
        )
        board.rows


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
        rows =
            Array.slice y1 y2 board.rows |> Array.map (Array.slice x1 x2)
    in
    { width = x2 - x1, height = y2 - y1, rows = rows }


lockPiece : Piece -> Position -> Board -> Board
lockPiece piece position board =
    let
        indexedPieces =
            piece
                |> Piece.indexedMap (\x y square -> ( x, y, square ))
                |> List.concat

        newRows =
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
                    board.rows
    in
    { board | rows = newRows }


compact : Board -> ( Int, Board )
compact board =
    let
        newRows =
            Array.filter
                (Array.toList >> List.all ((==) Occupied) >> not)
                board.rows

        removedRows =
            board.height - Array.length newRows

        replacementRows =
            Array.repeat removedRows <|
                Array.repeat board.width Empty
    in
    ( removedRows
    , { board | rows = Array.append replacementRows newRows }
    )
