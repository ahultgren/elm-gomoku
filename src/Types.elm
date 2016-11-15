module Types exposing (..)

import Dict exposing (Dict)


type Msg
    = TileClick Coords
    | UndoHistory
    | Resize Int
    | ServerMessage String
    | Move Coords
    | StartLocalGame
    | StartOnlineGame Player
    | JoinOnlineGame


type Mark
    = EmptyTile
    | TakenTile Player


type Player
    = PlayerOne
    | PlayerTwo


type GameState
    = NotStarted
    | Pending
    | Started (List Player) Bool
    | Finished Player
    | Disconnected


type alias Model =
    { state : GameState
    , boardSize : Int
    , gridSize : Int
    , marks : Marks
    , currentPlayer : Player
    , history : List Coords
    , wsAddress : String
    }


type alias Coords =
    ( Int, Int )


type alias Marks =
    Dict Int Row


type alias Row =
    Dict Int Mark
