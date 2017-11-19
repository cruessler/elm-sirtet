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


type Mode
    = Tetris
    | Sirtet


type alias Model =
    { game : Maybe Game
    , mode : Mode
    , keybindings : Dict Char GameMsg
    , rebindKey : Maybe Char
    }


type GameMsg
    = Turn Piece.Direction
    | Move Game.Direction
    | Drop
    | Pause
    | Resume
    | Restart


type Msg
    = NewGame Time
    | Tick Time
    | KeyPress Char
    | RebindKey Char
    | SetMode Mode
    | GameMsg GameMsg


main =
    H.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( { game = Nothing
      , mode = Tetris
      , keybindings = initialKeybindings
      , rebindKey = Nothing
      }
    , Cmd.none
    )


columns : Int
columns =
    14


rows : Int
rows =
    20


initialKeybindings : Dict Char GameMsg
initialKeybindings =
    [ ( 'S', Move Left )
    , ( 'F', Move Right )
    , ( 'D', Move Down )
    , ( 'K', Turn Clockwise )
    , ( 'J', Turn Counterclockwise )
    , ( ' ', Drop )
    , ( 'P', Pause )
    , ( 'R', Resume )
    , ( 'A', Restart )
    ]
        |> Dict.fromList


rebindKey : Char -> Char -> Dict Char GameMsg -> Dict Char GameMsg
rebindKey oldKey newKey bindings =
    case
        ( Dict.member newKey bindings
        , Dict.get oldKey bindings
        )
    of
        ( False, Just binding ) ->
            bindings
                |> Dict.remove oldKey
                |> Dict.insert newKey binding

        _ ->
            bindings


updateGame : GameMsg -> Model -> ( Model, Cmd Msg )
updateGame msg model =
    let
        f =
            case msg of
                Turn direction ->
                    Game.turnPiece direction

                Move direction ->
                    Game.movePiece direction

                Drop ->
                    Game.dropPiece

                Pause ->
                    Game.pause

                Resume ->
                    Game.resume

                _ ->
                    identity

        cmd =
            case msg of
                Restart ->
                    Task.perform NewGame Time.now

                _ ->
                    Cmd.none
    in
        ( { model | game = Maybe.map f model.game }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame now ->
            let
                game =
                    Random.initialSeed (round now)
                        |> Game.initialize rows columns
            in
                ( { model | game = Just game }, Cmd.none )

        Tick _ ->
            ( { model | game = Maybe.map Game.step model.game }
            , Cmd.none
            )

        KeyPress key ->
            case model.rebindKey of
                Just oldKey ->
                    ( { model
                        | rebindKey = Nothing
                        , keybindings = rebindKey oldKey key model.keybindings
                      }
                    , Cmd.none
                    )

                Nothing ->
                    case Dict.get key model.keybindings of
                        Just msg ->
                            updateGame msg model

                        Nothing ->
                            ( model, Cmd.none )

        RebindKey key ->
            if Dict.member key model.keybindings then
                ( { model
                    | rebindKey = Just key
                    , game = Maybe.map Game.pause model.game
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        SetMode mode ->
            ( { model | mode = mode }, Cmd.none )

        GameMsg msg ->
            updateGame msg model


msPerFrame : Float
msPerFrame =
    1000.0 / 60.0


{-| The interval between ticks.

This function returns values between `5.0 * msPerFrame` and `45.0 *
msPerFrame`.

-}
interval : Int -> Float
interval round =
    (msPerFrame * 45.0) - min (msPerFrame * 40.0) (round * 2 |> toFloat)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        downs =
            Keyboard.downs (Char.fromCode >> KeyPress)
    in
        case model.game of
            Just (Running game) ->
                Sub.batch
                    [ Time.every (Time.millisecond * interval game.round) Tick
                    , downs
                    ]

            _ ->
                downs


infoBoard : Board
infoBoard =
    Board.initialize 4 4 (\_ _ -> Empty)


info : Int -> Int -> Int -> Maybe Piece -> Mode -> Html Msg
info points round removedRows nextPiece mode =
    let
        squares : Piece -> Dict ( Int, Int ) (List ( String, Bool ))
        squares piece =
            squaresForBoard infoBoard
                |> Dict.union (squaresForPiece { x = 0, y = 0 } piece)

        radio : Mode -> String -> Html Msg
        radio mode_ text =
            H.label []
                [ H.input
                    [ A.type_ "radio"
                    , A.checked (mode == mode_)
                    , E.onClick (SetMode mode_)
                    ]
                    []
                , H.text text
                ]
    in
        H.div [ A.id "info" ]
            [ H.div [] [ H.text ("Points " ++ toString points) ]
            , H.div [] [ H.text ("Round " ++ toString round) ]
            , H.div [] [ H.text ("Removed rows " ++ toString removedRows) ]
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
            , H.div [] [ radio Tetris "Tetris" ]
            , H.div [] [ radio Sirtet "Sirtet" ]
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


key : String -> Maybe Char -> Char -> Html Msg
key description rebindKey code =
    let
        readable : String -> String
        readable s =
            case s of
                " " ->
                    "Space"

                _ ->
                    s
    in
        H.div []
            [ H.kbd
                [ A.classList [ ( "rebind-key", rebindKey == Just code ) ]
                , E.onClick <| RebindKey code
                ]
                [ H.text <| (String.fromChar >> readable) code ]
            , H.text description
            ]


helpMessage : GameMsg -> String
helpMessage msg =
    case msg of
        Move Left ->
            "Move piece to the left"

        Move Right ->
            "Move piece to the right"

        Move Down ->
            "Move piece down"

        Turn Clockwise ->
            "Turn piece clockwise"

        Turn Counterclockwise ->
            "Turn piece counterclockwise"

        Drop ->
            "Drop piece"

        Pause ->
            "Pause game"

        Resume ->
            "Resume game"

        Restart ->
            "Start new game"


help : Maybe Char -> Dict Char GameMsg -> Html Msg
help rebindKey bindings =
    let
        boundTo : GameMsg -> Maybe Char
        boundTo msg =
            bindings
                |> Dict.filter (\k msg_ -> msg == msg_)
                |> Dict.keys
                |> List.head

        children =
            [ Move Left
            , Move Right
            , Move Down
            , Turn Clockwise
            , Turn Counterclockwise
            , Drop
            , Pause
            , Resume
            , Restart
            ]
                |> List.filterMap
                    (\msg ->
                        boundTo msg
                            |> Maybe.map (key (helpMessage msg) rebindKey)
                    )
    in
        H.div [ A.id "help" ]
            (H.p [ A.class "explanation" ]
                [ H.text "Click on a key to rebind it" ]
                :: children
            )


onTouchStart : msg -> H.Attribute msg
onTouchStart msg =
    E.onWithOptions "touchstart"
        { stopPropagation = True, preventDefault = True }
        (Decode.succeed msg)


tapAreas : Html Msg
tapAreas =
    H.div [ A.class "touch-areas" ]
        [ H.div [ A.class "turn-piece", onTouchStart <| GameMsg (Turn Clockwise) ] []
        , H.div [ A.class "move-left", onTouchStart <| GameMsg (Move Left) ] []
        , H.div [ A.class "move-right", onTouchStart <| GameMsg (Move Right) ] []
        , H.div [ A.class "move-down", onTouchStart <| GameMsg (Move Down) ] []
        , H.div [ A.class "drop-piece", onTouchStart <| GameMsg Drop ] []
        ]


{-| This function uses a <style> tag to set custom CSS properties (
<https://developer.mozilla.org/en-US/docs/Web/CSS/--*>).

This is necessary since custom properties currently (November 2017) cannot be
set using `Html.Attributes.style` because of its implementation in
`elm-lang/virtual-dom` (the library underlying `elm-lang/html`). `virtual-dom`
uses `.style[…]` to set CSS properties which doesn’t permit setting *custom*
properties. If it used `setProperty` instead, setting custom CSS properties
would be possible, but currently it does not seem likely that it will ever do
that. See
<https://github.com/elm-lang/virtual-dom/pull/44#issuecomment-313625843>.

See further
<https://www.reddit.com/r/elm/comments/6qn5c2/setting_css_variables_in_elm/>
<https://ellie-app.com/3TqZ5S3pGyQa1/0>

These properties are used to compute CSS values depending on screen orientation
or width (using media queries). This is simple using CSS, but would require
considerable effort when done in pure Elm because inline styles cannot contain
media queries.

-}
content : List (Html Msg) -> Html Msg
content children =
    let
        variables =
            H.node "style"
                []
                [ H.text <|
                    ".variables { --rows: "
                        ++ (toString rows)
                        ++ "; --columns: "
                        ++ (toString columns)
                        ++ "; }"
                ]
    in
        H.main_
            [ A.class "variables" ]
            (children ++ [ variables, tapAreas ])


grid : List (H.Attribute Msg) -> Mode -> List (Html Msg) -> Html Msg
grid attributes mode children =
    H.div
        ([ A.id "board"
         , A.classList [ ( "upside-down", mode == Sirtet ) ]
         ]
            ++ attributes
        )
        children


startButton : Html Msg
startButton =
    H.div [ A.class "center" ]
        [ H.button [ E.onClick <| GameMsg Restart ] [ H.text "Start new game" ]
        , H.p []
            [ H.small []
                [ H.text "If you are using a touch device, you can use different areas of the screen to control the game" ]
            ]
        , H.div [ A.class "show-touch-areas" ]
            [ H.div [ A.class "turn-piece" ] []
            , H.div [ A.class "move-left" ] []
            , H.div [ A.class "move-right" ] []
            , H.div [ A.class "move-down" ] []
            , H.div [ A.class "drop-piece" ] []
            ]
        ]


resumeButton : Html Msg
resumeButton =
    H.div [ A.class "center" ]
        [ H.button [ E.onClick <| GameMsg Resume ] [ H.text "Resume game" ]
        ]


view : Model -> Html Msg
view model =
    case model.game of
        Just (Running game) ->
            content
                [ (board game.position game.piece game.board)
                    |> grid [] model.mode
                , info
                    game.points
                    game.round
                    game.removedRows
                    (Just game.nextPiece)
                    model.mode
                , help model.rebindKey model.keybindings
                ]

        Just (Paused game) ->
            content
                [ (board game.position game.piece game.board)
                    |> grid [] model.mode
                , info
                    game.points
                    game.round
                    game.removedRows
                    (Just game.nextPiece)
                    model.mode
                , resumeButton
                , help model.rebindKey model.keybindings
                ]

        Just (Lost game) ->
            content
                [ (lostBoard game.board)
                    |> grid [ A.class "lost" ] model.mode
                , info
                    game.points
                    game.round
                    game.removedRows
                    Nothing
                    model.mode
                , startButton
                , help model.rebindKey model.keybindings
                ]

        _ ->
            content
                [ H.div [ A.id "board" ] []
                , startButton
                , help model.rebindKey model.keybindings
                ]
