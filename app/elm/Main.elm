module Main exposing (main)

import Game exposing (Game)
import Html as H exposing (Html)
import Random
import Task
import Time exposing (Time)


type alias Model =
    Maybe Game


type Msg
    = NewGame Time


main =
    H.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( Nothing, Task.perform NewGame Time.now )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame now ->
            let
                game =
                    Game.initialize <| Random.initialSeed (round now)
            in
                ( Just game, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


view : Model -> Html Msg
view model =
    H.div [] []
