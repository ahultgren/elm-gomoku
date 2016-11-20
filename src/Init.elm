module Init exposing (..)

import Dict
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


initFromModel : Model -> ( Model, Cmd x )
initFromModel model =
    init
        { size =
            { width = model.boardSize
            , height = model.boardSize + 100
            }
        , wsAddress = model.wsAddress
        }


getBoardSize : WindowSize -> Int
getBoardSize { height, width } =
    if height - 100 < width then
        height - 100
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
