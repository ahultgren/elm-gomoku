module Gomoku exposing (..)

import Dict
import Html.App exposing (programWithFlags)
import Window
import WebSocket
import Update exposing (update)
import View exposing (view)
import Types
    exposing
        ( Model
        , Marks
        , Row
        , Msg(TileClick, Resize, ServerMessage)
        , Mark(EmptyTile, TakenTile)
        , Player(PlayerOne, PlayerTwo)
        )


type alias InitModel =
    { size : WindowSize
    , wsAdress : String
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


main : Program InitModel
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
        , WebSocket.listen model.wsAdress ServerMessage
        ]


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
    ( { boardSize = getBoardSize state.size
      , gridSize = initGridSize
      , marks = generateEmptyBoard initGridSize
      , currentPlayer = PlayerOne
      , hasWon = Nothing
      , history = []
      , wsAdress = state.wsAdress
      }
    , Cmd.none
    )
