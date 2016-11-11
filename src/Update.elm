module Update exposing (..)

import Maybe.Extra exposing (join)
import Dict
import Json.Decode as Decode exposing (decodeString)
import Json.Encode as Encode
import WebSocket
import Decoder exposing (decodeCommand, Command, EnumType(EnumTypeMove))
import Types
    exposing
        ( Model
        , Marks
        , Row
        , Msg(TileClick, UndoHistory, Resize, ServerMessage, Move)
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
                ( model
                    |> move coords
                    |> checkWinCondition coords
                    |> updateHistory coords
                , sendMove coords model
                )

        Move coords ->
            ( model
                |> move coords
                |> checkWinCondition coords
                |> updateHistory coords
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

        Resize width ->
            ( { model
                | boardSize = width
              }
            , Cmd.none
            )

        ServerMessage str ->
            decodeString decodeCommand (Debug.log "received" str)
                |> Result.map (handleServerCommand model)
                |> Result.withDefault ( model, Cmd.none )


move : Coords -> Model -> Model
move coords model =
    { model
        | marks = updateMarks (addMark model.currentPlayer) model.marks coords
        , currentPlayer = updatePlayer model.marks coords model.currentPlayer
    }


checkWinCondition : Coords -> Model -> Model
checkWinCondition start model =
    { model
        | hasWon = findWinner model start
    }


updateHistory : Coords -> Model -> Model
updateHistory coords model =
    { model
        | history = coords :: model.history
    }


sendMove : Coords -> Model -> Cmd Msg
sendMove coords model =
    WebSocket.send model.wsAdress
        (encodeCommand <|
            Command (Decoder.Move (fst coords) (snd coords)) EnumTypeMove
        )


handleServerCommand : Model -> Command -> ( Model, Cmd Msg )
handleServerCommand model { type_, coords } =
    -- check that it's the oppponent's turn
    update (Move ( coords.x, coords.y )) model


encodeCommand : Command -> String
encodeCommand command =
    Encode.encode 0
        (Encode.object
            [ ( "type", (encodeEnumType command.type_) )
            , ( "coords"
              , Encode.object
                    [ ( "x", Encode.int command.coords.x )
                    , ( "y", Encode.int command.coords.y )
                    ]
              )
            ]
        )


encodeEnumType : EnumType -> Encode.Value
encodeEnumType type_ =
    case type_ of
        EnumTypeMove ->
            Encode.string "Move"


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
