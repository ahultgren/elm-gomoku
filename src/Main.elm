module Gomoku exposing (..)

import Dict
import Html.App exposing (programWithFlags)
import Window
import Update exposing (update)
import View exposing (view)
import Types
    exposing
        ( Model
        , Marks
        , Row
        , Msg(TileClick, CheckWinCondition, Resize)
        , Mark(EmptyTile, TakenTile)
        , Player(PlayerOne, PlayerTwo)
        )


type alias WindowSize =
    { width : Int
    , height : Int
    }


main : Program WindowSize
main =
    programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Window.resizes (Resize << getBoardSize)


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


init : WindowSize -> ( Model, Cmd x )
init windowSize =
    ( { boardSize = getBoardSize windowSize
      , gridSize = initGridSize
      , marks = generateEmptyBoard initGridSize
      , currentPlayer = PlayerOne
      , hasWon = Nothing
      , history = []
      }
    , Cmd.none
    )
