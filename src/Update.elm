module Update exposing (..)

import Maybe.Extra exposing (join)
import Dict
import Types
    exposing
        ( Model
        , Marks
        , Row
        , Msg(TileClick, CheckWinCondition, PushHistory, UndoHistory)
        , Mark(EmptyTile, TakenTile)
        , Player(PlayerOne, PlayerTwo)
        , Coords
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TileClick (( x, y ) as coords) ->
            if x < 0 || y < 0 || x > model.gridSize || y > model.gridSize then
                ( model, Cmd.none )
            else
                let
                    newModel =
                        { model
                            | marks = updateMarks (addMark model.currentPlayer) model.marks coords
                            , currentPlayer = updatePlayer model.marks coords model.currentPlayer
                        }
                in
                    update (CheckWinCondition coords) newModel
                        |> (update (PushHistory coords) << fst)

        CheckWinCondition start ->
            ( { model
                | hasWon = findWinner model start
              }
            , Cmd.none
            )

        PushHistory coords ->
            ( { model
                | history = coords :: model.history
              }
            , Cmd.none
            )

        UndoHistory ->
            ( { model
                | history = List.drop 1 model.history
                , marks =
                    List.head model.history
                        |> Maybe.map (updateMarks removeMark model.marks)
                        |> Maybe.withDefault model.marks
                , currentPlayer = undoPlayer model.currentPlayer
              }
            , Cmd.none
            )


updateMarks : (Mark -> Mark) -> Marks -> Coords -> Marks
updateMarks updater marks ( x, y ) =
    Dict.update x (Maybe.map <| updateRow updater y) marks


updateRow : (Mark -> Mark) -> Int -> Row -> Row
updateRow updater y row =
    Dict.update y (Maybe.map updater) row


addMark : Player -> Mark -> Mark
addMark player mark =
    case mark of
        EmptyTile ->
            TakenTile player

        TakenTile _ ->
            mark


removeMark : Mark -> Mark
removeMark mark =
    case mark of
        EmptyTile ->
            mark

        TakenTile _ ->
            EmptyTile


updatePlayer : Marks -> Coords -> Player -> Player
updatePlayer marks coords player =
    case getMark coords marks of
        Nothing ->
            player

        Just (TakenTile _) ->
            player

        Just EmptyTile ->
            case player of
                PlayerOne ->
                    PlayerTwo

                PlayerTwo ->
                    PlayerOne


getMark : Coords -> Marks -> Maybe Mark
getMark ( x, y ) marks =
    Dict.get x marks
        |> Maybe.map (Dict.get y)
        |> join


type Direction
    = Up
    | UpRight
    | Right
    | DownRight
    | Down
    | DownLeft
    | Left
    | UpLeft


dirToCoords : Coords -> Direction -> Coords
dirToCoords ( x, y ) dir =
    case dir of
        Up ->
            ( x, y - 1 )

        UpRight ->
            ( x + 1, y - 1 )

        Right ->
            ( x + 1, y )

        DownRight ->
            ( x + 1, y + 1 )

        Down ->
            ( x, y + 1 )

        DownLeft ->
            ( x - 1, y + 1 )

        Left ->
            ( x - 1, y )

        UpLeft ->
            ( x - 1, y - 1 )


findWinner : Model -> Coords -> Maybe Player
findWinner { marks } coords =
    let
        dirs =
            [ ( Up, Down ), ( UpRight, DownLeft ), ( Right, Left ), ( DownRight, UpLeft ) ]

        score =
            List.map (sumDirs coords marks) dirs
    in
        if List.any ((<=) 5) score then
            getMark coords marks
                |> Maybe.map markToPlayer
                |> join
        else
            Nothing


sumDirs : Coords -> Marks -> ( Direction, Direction ) -> Int
sumDirs coords marks ( dir1, dir2 ) =
    1
        + (search 0 coords marks dir1)
        + (search 0 coords marks dir2)


search : Int -> Coords -> Marks -> Direction -> Int
search score coords marks dir =
    let
        nextCoords =
            dirToCoords coords dir

        lastMark =
            getMark coords marks

        nextMark =
            getMark nextCoords marks
    in
        if lastMark == nextMark then
            search (score + 1) nextCoords marks dir
        else
            score


markToPlayer : Mark -> Maybe Player
markToPlayer mark =
    case mark of
        TakenTile player ->
            Just player

        EmptyTile ->
            Nothing


undoPlayer : Player -> Player
undoPlayer player =
    case player of
        PlayerOne ->
            PlayerTwo

        PlayerTwo ->
            PlayerOne
