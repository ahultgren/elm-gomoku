module Gomoku exposing (..)

import Dict
import Html.App exposing (program)
import Update exposing (update)
import View exposing (view)
import Types
    exposing
        ( Model
        , Marks
        , Row
        , Msg(TileClick, CheckWinCondition)
        , Mark(EmptyTile, TakenTile)
        , Player(PlayerOne, PlayerTwo)
        )


main : Program Never
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions x =
    Sub.none


generateEmptyBoard : Int -> Marks
generateEmptyBoard count =
    Dict.fromList <|
        List.indexedMap (,) <|
            List.repeat count <|
                Dict.fromList <|
                    List.indexedMap (,) <|
                        List.repeat count EmptyTile


initGridSize : Int
initGridSize =
    19


init : ( Model, Cmd x )
init =
    ( { width = 500
      , height = 500
      , gridSize = initGridSize
      , marks = generateEmptyBoard initGridSize
      , currentPlayer = PlayerOne
      , hasWon = Nothing
      }
    , Cmd.none
    )
