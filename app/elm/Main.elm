module Main exposing (main)

import Array exposing (Array)
import Board exposing (Board, Position)
import Char
import Dict exposing (Dict)
import Game exposing (Game(..), Direction(..))
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Keyboard
import Json.Decode as Decode
import Piece exposing (Piece, Square(..), Direction(..))
import Random
import Task
import Time exposing (Time)


type alias Model =
    Maybe Game


type Msg
    = StartGame
    | NewGame Time
    | ResumeGame
    | Tick Time
    | KeyPress Char


main =
    H.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.none )


columns : Int
columns =
    14


rows : Int
rows =
    20


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( model, Task.perform NewGame Time.now )

        NewGame now ->
            let
                game =
                    Random.initialSeed (round now)
                        |> Game.initialize rows columns
            in
                ( Just game, Cmd.none )

        ResumeGame ->
            ( Maybe.map Game.resume model, Cmd.none )

        Tick _ ->
            ( Maybe.map Game.step model, Cmd.none )

        KeyPress key ->
            let
                f =
                    case key of
                        'S' ->
                            Game.movePiece Left

                        'F' ->
                            Game.movePiece Right

                        'D' ->
                            Game.movePiece Down

                        'K' ->
                            Game.turnPiece Clockwise

                        'J' ->
                            Game.turnPiece Counterclockwise

                        ' ' ->
                            Game.dropPiece

                        'P' ->
                            Game.pause

                        'R' ->
                            Game.resume

                        _ ->
                            identity

                cmd =
                    if key == 'A' then
                        Task.perform NewGame Time.now
                    else
                        Cmd.none
            in
                ( Maybe.map f model, cmd )


interval : Float
interval =
    700.0


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        downs =
            Keyboard.downs (Char.fromCode >> KeyPress)
    in
        case model of
            Just (Running _) ->
                Sub.batch
                    [ Time.every (Time.millisecond * interval) Tick
                    , downs
                    ]

            _ ->
                downs


infoBoard : Board
infoBoard =
    Board.initialize 4 4 (\_ _ -> Empty)


info : Int -> Int -> Maybe Piece -> Html Msg
info points round nextPiece =
    let
        squares : Piece -> Dict ( Int, Int ) (List ( String, Bool ))
        squares piece =
            squaresForBoard infoBoard
                |> Dict.union (squaresForPiece { x = 0, y = 0 } piece)
    in
        H.div [ A.id "info" ]
            [ H.div [] [ H.text ("Points " ++ toString points) ]
            , H.div [] [ H.text ("Round " ++ toString round) ]
            , H.div [ A.id "next-piece" ]
                (nextPiece
                    |> Maybe.map
                        (squares
                            >> Dict.map
                                (\_ classList ->
                                    H.div [ (( "square", True ) :: classList) |> A.classList ] []
                                )
                            >> Dict.values
                        )
                    |> Maybe.withDefault []
                )
            ]


squaresForBoard : Board -> Dict ( Int, Int ) (List ( String, Bool ))
squaresForBoard board =
    board
        |> Board.indexedMap (\x y square -> ( x, y, square ))
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat
        |> List.foldl
            (\( x, y, square ) acc ->
                case (square) of
                    Occupied ->
                        Dict.insert ( y, x ) [ ( "occupied", True ) ] acc

                    Empty ->
                        Dict.insert ( y, x ) [] acc
            )
            Dict.empty


squaresForPiece : Position -> Piece -> Dict ( Int, Int ) (List ( String, Bool ))
squaresForPiece position piece =
    piece
        |> Piece.indexedMap (\x y square -> ( x, y, square ))
        |> List.concat
        |> List.foldl
            (\( x, y, square ) acc ->
                case square of
                    Occupied ->
                        Dict.insert
                            ( position.y + y, position.x + x )
                            [ ( "piece", True ), ( "occupied", True ) ]
                            acc

                    Empty ->
                        acc
            )
            Dict.empty


board : Position -> Piece -> Board -> List (Html Msg)
board position piece board =
    squaresForBoard board
        |> Dict.union (squaresForPiece position piece)
        |> Dict.map
            (\_ classList ->
                H.div [ (( "square", True ) :: classList) |> A.classList ] []
            )
        |> Dict.values


lostBoard : Board -> List (Html Msg)
lostBoard board =
    squaresForBoard board
        |> Dict.map
            (\_ classList ->
                H.div [ (( "square", True ) :: classList) |> A.classList ] []
            )
        |> Dict.values


key : String -> String -> Html Msg
key code description =
    H.div []
        [ H.kbd [] [ H.text code ]
        , H.text description
        ]


help : Html Msg
help =
    H.div [ A.id "help" ]
        [ key "a" "Start new game"
        , key "p" "Pause game"
        , key "r" "Resume game"
        , key "s" "Move piece to the left"
        , key "f" "Move piece to the right"
        , key "d" "Move piece down"
        , key "k" "Turn piece clockwise"
        , key "j" "Turn piece counterclockwise"
        , key "Space" "Drop piece"
        ]


content : List (Html Msg) -> Html Msg
content children =
    H.main_
        [ A.style
            [ ( "width", (toString <| (columns + 4) * 2) ++ "em" ) ]
        ]
        (children ++ [ help ])


grid : List (H.Attribute Msg) -> List (Html Msg) -> Html Msg
grid attributes children =
    H.div
        ([ A.id "board"
         , A.style
            [ ( "height", (toString <| rows * 2) ++ "em" )
            , ( "grid-template-columns", "repeat(" ++ (toString columns) ++ ", 1fr)" )
            , ( "grid-template-rows", "repeat(" ++ (toString rows) ++ ", 1fr)" )
            ]
         ]
            ++ attributes
        )
        children


startButton : Html Msg
startButton =
    H.button
        [ A.style
            [ ( "top", (toString <| round (toFloat (rows * 2) / 2)) ++ "em" )
            ]
        , E.onClick StartGame
        ]
        [ H.text "Start new game" ]


resumeButton : Html Msg
resumeButton =
    H.button
        [ A.style
            [ ( "top", (toString <| round (toFloat (rows * 2) / 2)) ++ "em" )
            ]
        , E.onClick ResumeGame
        ]
        [ H.text "Resume game" ]


view : Model -> Html Msg
view model =
    case model of
        Just (Running game) ->
            content
                [ info game.points game.round (Just game.nextPiece)
                , (board game.position game.piece game.board)
                    |> grid []
                ]

        Just (Paused game) ->
            content
                [ info game.points game.round (Just game.nextPiece)
                , (board game.position game.piece game.board)
                    |> grid []
                , resumeButton
                ]

        Just (Lost game) ->
            content
                [ info game.points game.round Nothing
                , (lostBoard game.board)
                    |> grid [ A.class "lost" ]
                , startButton
                ]

        _ ->
            content
                [ H.div
                    [ A.id "board"
                    , A.style [ ( "height", (toString <| rows * 2) ++ "em" ) ]
                    ]
                    []
                , startButton
                ]
