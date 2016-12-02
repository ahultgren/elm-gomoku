module Update exposing (..)

import Maybe.Extra exposing (join)
import Dict
import Json.Decode as Decode exposing (decodeString)
import Json.Encode as Encode
import WebSocket
import Init exposing (initFromModel)
import Decoder exposing (decodeCommand, decodeMove, decodeStart, Command, EnumType(EnumTypeMove, EnumTypeStart, EnumTypeDisconnected))
import Types
    exposing
        ( Model
        , GameState(NotStarted, Pending, Started, Finished, OpponentLeft)
        , Marks
        , Row
        , Msg(TileClick, UndoHistory, Resize, ServerMessage, StartLocalGame, StartOnlineGame, JoinOnlineGame, Reset)
        , Mark(EmptyTile, TakenTile)
        , Player(PlayerOne, PlayerTwo)
        , Coords
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartLocalGame ->
            ( { model | state = Started [ PlayerOne, PlayerTwo ] False }, Cmd.none )

        JoinOnlineGame ->
            ( { model | state = Pending }, Cmd.none )

        StartOnlineGame player ->
            ( { model | state = Started [ player ] True }, Cmd.none )

        TileClick coords ->
            case model.state of
                Started allowedPlayers online ->
                    moveLocal coords allowedPlayers online model

                _ ->
                    ( model, Cmd.none )

        UndoHistory ->
            ( undo model, Cmd.none )

        Resize width ->
            ( { model | boardSize = width }, Cmd.none )

        ServerMessage json ->
            handleServerMessage json model

        Reset ->
            initFromModel model


undo : Model -> Model
undo model =
    { model
        | history = List.drop 1 model.history
        , marks =
            List.head model.history
                |> Maybe.map (updateMarks removeMark model.marks)
                |> Maybe.withDefault model.marks
        , currentPlayer = undoPlayer model.currentPlayer
    }


currentPlayerMayMove : List Player -> Player -> Bool
currentPlayerMayMove allowedPlayers currentPlayer =
    List.any ((==) currentPlayer) allowedPlayers


moveLocal : Coords -> List Player -> Bool -> Model -> ( Model, Cmd Msg )
moveLocal (( x, y ) as coords) allowedPlayers online model =
    if currentPlayerMayMove allowedPlayers model.currentPlayer then
        if x < 0 || y < 0 || x > model.gridSize || y > model.gridSize then
            ( model, Cmd.none )
        else
            ( move coords model
            , sendMove online coords model
            )
    else
        ( model, Cmd.none )


move : Coords -> Model -> Model
move coords model =
    { model
        | marks = updateMarks (addMark model.currentPlayer) model.marks coords
        , currentPlayer = updatePlayer model.marks coords model.currentPlayer
    }
        |> checkWinCondition coords
        |> updateHistory coords


checkWinCondition : Coords -> Model -> Model
checkWinCondition start model =
    case findWinner model start of
        Nothing ->
            model

        Just player ->
            { model
                | state = Finished player
            }


updateHistory : Coords -> Model -> Model
updateHistory coords model =
    { model
        | history = coords :: model.history
    }


sendMove : Bool -> Coords -> Model -> Cmd Msg
sendMove online coords model =
    if online then
        WebSocket.send model.wsAddress
            (encodeMove <|
                Decoder.Coords (Tuple.first coords) (Tuple.second coords)
            )
    else
        Cmd.none


handleServerMessage : String -> Model -> ( Model, Cmd Msg )
handleServerMessage json model =
    decodeString decodeCommand (Debug.log "received" json)
        |> Result.map (handleServerCommand json model)
        |> Result.withDefault ( model, Cmd.none )


handleServerCommand : String -> Model -> Command -> ( Model, Cmd Msg )
handleServerCommand json model { type_ } =
    case type_ of
        Decoder.EnumTypeStart ->
            decodeString decodeStart json
                |> Result.map
                    (\{ player } ->
                        update
                            (StartOnlineGame (enumPlayerToPlayer player))
                            model
                    )
                |> Result.withDefault ( model, Cmd.none )

        Decoder.EnumTypeMove ->
            -- check that it's the oppponent's turn
            ( decodeString decodeMove json
                |> Result.map
                    (\{ coords } ->
                        move ( coords.x, coords.y ) model
                    )
                |> Result.withDefault model
            , Cmd.none
            )

        Decoder.EnumTypeDisconnected ->
            ( { model
                | state = OpponentLeft
              }
            , Cmd.none
            )


enumPlayerToPlayer : Decoder.EnumPlayer -> Player
enumPlayerToPlayer player =
    (case player of
        Decoder.EnumPlayerPlayerOne ->
            PlayerOne

        Decoder.EnumPlayerPlayerTwo ->
            PlayerTwo
    )


encodeMove : Decoder.Coords -> String
encodeMove coords =
    Encode.encode 0
        (Encode.object
            [ ( "type", (encodeEnumType EnumTypeMove) )
            , ( "coords"
              , Encode.object
                    [ ( "x", Encode.int coords.x )
                    , ( "y", Encode.int coords.y )
                    ]
              )
            ]
        )


encodeEnumType : EnumType -> Encode.Value
encodeEnumType type_ =
    case type_ of
        EnumTypeMove ->
            Encode.string "Move"

        EnumTypeStart ->
            Encode.string "Start"

        EnumTypeDisconnected ->
            Encode.string "Disconnected"


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
