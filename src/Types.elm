module Types exposing (..)

import Dict exposing (Dict)


type Msg
    = TileClick ( Int, Int )
    | CheckWinCondition ( Int, Int )


type Mark
    = EmptyTile
    | TakenTile Player


type Player
    = PlayerOne
    | PlayerTwo


type alias Model =
    { width : Float
    , height : Float
    , gridSize : Int
    , marks : Marks
    , currentPlayer : Player
    , hasWon : Maybe Player
    }


type alias Marks =
    Dict Int Row


type alias Row =
    Dict Int Mark
