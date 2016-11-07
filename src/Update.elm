module Update exposing (..)

import Maybe.Extra exposing (join)
import Dict
import Types
    exposing
        ( Model
        , Marks
        , Row
        , Msg(TileClick, CheckWinCondition)
        , Mark(EmptyTile, TakenTile)
        , Player(PlayerOne, PlayerTwo)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TileClick ( x, y ) ->
            if x < 0 || y < 0 || x > model.gridSize || y > model.gridSize then
                ( model, Cmd.none )
            else
                let
                    newModel =
                        { model
                            | marks = updateMarks ( x, y ) model.currentPlayer model.marks
                            , currentPlayer = updatePlayer model.marks ( x, y ) model.currentPlayer
                        }
                in
                    update (CheckWinCondition ( x, y )) newModel

        CheckWinCondition start ->
            ( { model
                | hasWon = findWinner model start
              }
            , Cmd.none
            )


updateMarks : ( Int, Int ) -> Player -> Marks -> Marks
updateMarks ( x, y ) player marks =
    Dict.update x (Maybe.map <| updateRow y player) marks


updateRow : Int -> Player -> Row -> Row
updateRow y player row =
    Dict.update y (Maybe.map <| updateMark player) row


updateMark : Player -> Mark -> Mark
updateMark player mark =
    case mark of
        EmptyTile ->
            TakenTile player

        TakenTile _ ->
            mark


updatePlayer : Marks -> ( Int, Int ) -> Player -> Player
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


getMark : ( Int, Int ) -> Marks -> Maybe Mark
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


dirToCoords : ( Int, Int ) -> Direction -> ( Int, Int )
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


findWinner : Model -> ( Int, Int ) -> Maybe Player
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


sumDirs : ( Int, Int ) -> Marks -> ( Direction, Direction ) -> Int
sumDirs coords marks ( dir1, dir2 ) =
    1
        + (search 0 coords marks dir1)
        + (search 0 coords marks dir2)


search : Int -> ( Int, Int ) -> Marks -> Direction -> Int
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
