module Piece
    exposing
        ( Piece
        , Square(..)
        , empty
        , all
        , random
        , width
        , height
        , Direction(..)
        , turn
        )

import Array exposing (Array)
import Random exposing (Seed)


type Square
    = Occupied
    | Empty


type alias Piece =
    List (List Square)


empty : Piece
empty =
    []


all : Array Piece
all =
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


random : Seed -> ( Piece, Seed )
random seed =
    let
        generator : Random.Generator Piece
        generator =
            Random.int 0 (Array.length all - 1)
                |> Random.map
                    (\i ->
                        all
                            |> Array.get i
                            |> Maybe.withDefault empty
                    )
    in
        Random.step generator seed


width : Piece -> Int
width =
    List.map List.length >> List.maximum >> Maybe.withDefault 0


height : Piece -> Int
height =
    List.length


type Direction
    = Clockwise
    | Counterclockwise


{-| This is a translation of
<http://hackage.haskell.org/package/base-4.10.0.0/docs/src/Data.OldList.html#transpose>
-}
transpose : Piece -> Piece
transpose piece =
    case piece of
        [] ->
            []

        [] :: row ->
            transpose row

        (first :: rest) :: row ->
            (first :: List.map (List.head >> Maybe.withDefault Empty) row)
                :: transpose (rest :: List.map (List.tail >> Maybe.withDefault []) row)


turn : Direction -> Piece -> Piece
turn direction =
    let
        f =
            List.reverse >> transpose
    in
        case direction of
            Clockwise ->
                f

            Counterclockwise ->
                f >> f >> f
