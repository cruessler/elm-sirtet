module Board
    exposing
        ( Square(..)
        , Board
        , Piece
        , Position
        , initialize
        , rows
        , columns
        , pieces
        , emptyBoard
        , isLegalPosition
        , slice
        , lockPiece
        )

import Array exposing (Array)


type Square
    = Occupied
    | Empty


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


emptyBoard : Board
emptyBoard =
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


type alias Piece =
    List (List Square)


pieces : Array Piece
pieces =
    [ [ [ Occupied, Occupied, Occupied, Occupied ] ]
    , [ [ Occupied, Empty, Empty ]
      , [ Occupied, Occupied, Occupied ]
      ]
    , [ [ Empty, Empty, Occupied ]
      , [ Occupied, Occupied, Occupied ]
      ]
    , [ [ Occupied, Occupied ]
      , [ Occupied, Occupied ]
      ]
    , [ [ Empty, Occupied, Occupied ]
      , [ Occupied, Occupied, Empty ]
      ]
    , [ [ Empty, Occupied, Empty ]
      , [ Occupied, Occupied, Occupied ]
      ]
    , [ [ Occupied, Occupied, Empty ]
      , [ Empty, Occupied, Occupied ]
      ]
    ]
        |> Array.fromList


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
