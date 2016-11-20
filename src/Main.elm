port module Gomoku exposing (..)

import Html exposing (programWithFlags)
import WebSocket
import Init exposing (InitModel, WindowSize, init, getBoardSize)
import Update exposing (update)
import View exposing (view)
import Types
    exposing
        ( Model
        , GameState(NotStarted, Pending, Started, Finished)
        , Marks
        , Row
        , Msg(TileClick, Resize, ServerMessage)
        , Mark(EmptyTile, TakenTile)
        , Player(PlayerOne, PlayerTwo)
        )


port resizes : (WindowSize -> msg) -> Sub msg


main : Program InitModel Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ resizes (Resize << getBoardSize)
        , wsSubscriptions model
        ]


wsSubscriptions : Model -> Sub Msg
wsSubscriptions model =
    case model.state of
        Pending ->
            WebSocket.listen model.wsAddress ServerMessage

        Started _ True ->
            WebSocket.listen model.wsAddress ServerMessage

        Finished _ ->
            WebSocket.listen model.wsAddress ServerMessage

        _ ->
            Sub.none
