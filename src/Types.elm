module Types exposing (..)

import Dict exposing (Dict)


type Msg
    = TileClick Coords
    | CheckWinCondition Coords
    | PushHistory Coords
    | UndoHistory
    | Resize Int


type Mark
    = EmptyTile
    | TakenTile Player


type Player
    = PlayerOne
    | PlayerTwo


type alias Model =
    { boardSize : Int
    , gridSize : Int
    , marks : Marks
    , currentPlayer : Player
    , hasWon : Maybe Player
    , history : List Coords
    }


type alias Coords =
    ( Int, Int )


type alias Marks =
    Dict Int Row


type alias Row =
    Dict Int Mark
