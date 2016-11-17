module Gomoku exposing (..)

import Dict
import Html exposing (programWithFlags)
import Window
import WebSocket
import Update exposing (update)
import View exposing (view)
import Types
    exposing
        ( Model
        , GameState(NotStarted, Pending, Started)
        , Marks
        , Row
        , Msg(TileClick, Resize, ServerMessage)
        , Mark(EmptyTile, TakenTile)
        , Player(PlayerOne, PlayerTwo)
        )


type alias InitModel =
    { size : WindowSize
    , wsAddress : String
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


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
        [ Window.resizes (Resize << getBoardSize)
        , wsSubscriptions model
        ]


wsSubscriptions : Model -> Sub Msg
wsSubscriptions model =
    case model.state of
        Pending ->
            WebSocket.listen model.wsAddress ServerMessage

        Started _ True ->
            WebSocket.listen model.wsAddress ServerMessage

        _ ->
            Sub.none


getBoardSize : WindowSize -> Int
getBoardSize { height, width } =
    if height < width then
        height
    else
        width


generateEmptyBoard : Int -> Marks
generateEmptyBoard count =
    List.repeat count EmptyTile
        |> List.indexedMap (,)
        |> Dict.fromList
        |> List.repeat count
        |> List.indexedMap (,)
        |> Dict.fromList


initGridSize : Int
initGridSize =
    19


init : InitModel -> ( Model, Cmd x )
init state =
    ( { state = NotStarted
      , boardSize = getBoardSize state.size
      , gridSize = initGridSize
      , marks = generateEmptyBoard initGridSize
      , currentPlayer = PlayerOne
      , history = []
      , wsAddress = state.wsAddress
      }
    , Cmd.none
    )
