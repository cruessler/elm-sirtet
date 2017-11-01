module Piece exposing (Piece, Square(..), empty, all, random, width, height)

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
