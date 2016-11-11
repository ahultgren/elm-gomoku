module Types exposing (..)

import Dict exposing (Dict)


type Msg
    = TileClick Coords
    | UndoHistory
    | Resize Int
    | ServerMessage String
    | Move Coords


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
    , wsAdress : String
    }


type alias Coords =
    ( Int, Int )


type alias Marks =
    Dict Int Row


type alias Row =
    Dict Int Mark
