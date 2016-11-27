module View exposing (..)

import Dict
import Json.Decode as Decode exposing (Decoder)
import Html exposing (Html, node, h1, text, div, button, br)
import Html.Attributes exposing (class)
import Html.Events exposing (on, onClick)
import Collage exposing (Form, collage, move, filled, outlined, rect, oval, group, polygon, defaultLine)
import Color exposing (Color, rgb, rgba)
import Element exposing (toHtml)
import Types
    exposing
        ( Model
        , GameState(NotStarted, Pending, Started, Finished, OpponentLeft)
        , Marks
        , Row
        , Msg(TileClick, UndoHistory, StartLocalGame, JoinOnlineGame, Reset)
        , Mark(EmptyTile, TakenTile)
        , Player(PlayerOne, PlayerTwo)
        , Coords
        )


decodeClickLocation : Decoder Coords
decodeClickLocation =
    Decode.map2 (,)
        (Decode.map2 (-)
            (Decode.at [ "pageX" ] Decode.int)
            (Decode.at [ "target", "offsetLeft" ] Decode.int)
        )
        (Decode.map2 (-)
            (Decode.at [ "pageY" ] Decode.int)
            (Decode.at [ "target", "offsetTop" ] Decode.int)
        )


coordsToTile : Int -> Int -> Coords -> Coords
coordsToTile boardSize count ( x, y ) =
    ( count * x // boardSize, count * y // boardSize )


view : Model -> Html Msg
view model =
    let
        content =
            case model.state of
                NotStarted ->
                    [ startScreen ]

                Pending ->
                    [ pendingView model ]

                Finished player ->
                    [ wonView player, boardView model ]

                OpponentLeft ->
                    [ leftView model, boardView model ]

                _ ->
                    [ boardView model ]
    in
        div [] ((stylesView model) :: content)


startScreen : Html Msg
startScreen =
    div
        [ class "screen screen--start"
        ]
        [ div [ class "start abs-cent" ]
            [ h1 [ class "title" ] [ text "Gomoku" ]
            , button
                [ class "btn"
                , onClick StartLocalGame
                ]
                [ text "Play local"
                ]
            , button
                [ class "btn"
                , onClick JoinOnlineGame
                ]
                [ text "Play online"
                ]
            , div
                [ class "start-rules"
                ]
                [ text "You win if you get five in a row"
                ]
            ]
        ]


pendingView : Model -> Html Msg
pendingView model =
    div
        [ class "screen screen--pending"
        ]
        [ div
            [ class "pending abs-cent"
            ]
            [ h1 [ class "title" ] [ text "Waiting for opponent..." ]
            , div [] [ text <| "Currently playing: " ++ (toString <| model.gameCount * 2) ]
            ]
        ]


wonView : Player -> Html Msg
wonView player =
    div [ class "screen screen--finished overlay" ]
        [ div [ class "abs-cent" ]
            [ h1 [ class "title" ]
                [ text <| (playerToString player) ++ " won!"
                ]
            , button [ class "btn", onClick Reset ] [ text "New game" ]
            ]
        ]


leftView : Model -> Html Msg
leftView model =
    div [ class "screen screen--left overlay" ]
        [ div [ class "left abs-cent" ]
            [ h1 [ class "title" ]
                [ text "Your opponent"
                , br [] []
                , text "disconnected :("
                ]
            , button [ class "btn", onClick Reset ] [ text "New game" ]
            ]
        ]


boardView : Model -> Html Msg
boardView model =
    div [ class "screen screen--started" ]
        [ div [ on "click" (Decode.map (coordsToTile model.boardSize model.gridSize >> TileClick) decodeClickLocation) ]
            [ toHtml <|
                collage
                    model.boardSize
                    model.boardSize
                    [ group <|
                        grid model.boardSize model.gridSize
                    , marksView model
                    ]
            ]
        , playerView model
        , undoView model
        ]


grid : Int -> Int -> List Form
grid boardSize count =
    let
        verticalLines =
            List.repeat (count + 1) ""
                |> List.indexedMap
                    (\i _ ->
                        moveInt ( i * boardSize // count - boardSize // 2, 0 ) <|
                            filled (rgb 3 3 3) <|
                                rect 1 (toFloat boardSize)
                    )

        horizontalLines =
            List.repeat (count + 1) ""
                |> List.indexedMap
                    (\i _ ->
                        moveInt ( 0, i * boardSize // count - boardSize // 2 ) <|
                            filled (rgb 1 1 1) <|
                                rect (toFloat boardSize) 1
                    )
    in
        List.append verticalLines horizontalLines


moveInt : ( Int, Int ) -> Form -> Form
moveInt ( x, y ) =
    move ( toFloat x, toFloat y )


playerView : Model -> Html Msg
playerView { currentPlayer, boardSize, state } =
    case state of
        Started _ False ->
            text ("Turn: " ++ playerToString currentPlayer)

        Started localPlayers True ->
            text
                (if List.any ((==) currentPlayer) localPlayers then
                    "Your turn (" ++ (playerToString currentPlayer) ++ ")"
                 else
                    "Opponent's turn (" ++ (playerToString currentPlayer) ++ ")"
                )

        _ ->
            text ""


playerToString : Player -> String
playerToString player =
    case player of
        PlayerOne ->
            "Crosses"

        PlayerTwo ->
            "Noughts"


marksView : Model -> Form
marksView model =
    Dict.map (rowView model) model.marks
        |> Dict.values
        |> group


rowView : Model -> Int -> Row -> Form
rowView model x row =
    Dict.map (markView model x) row
        |> Dict.values
        |> group


markColor : Color
markColor =
    rgb 48 48 48


activeMarkColor : Color
activeMarkColor =
    rgb 144 48 48


nought : Color -> Int -> Form
nought color boardSize =
    let
        diameter =
            toFloat <| boardSize // 50
    in
        outlined { defaultLine | color = color, width = 2 } <| oval diameter diameter


cross : Color -> Int -> Form
cross color boardSize =
    let
        corner =
            toFloat <| boardSize // 100
    in
        outlined { defaultLine | color = color, width = 2 } <|
            polygon [ ( 0, 0 ), ( -corner, -corner ), ( corner, corner ), ( 0, 0 ), ( corner, -corner ), ( -corner, corner ) ]


markView : Model -> Int -> Int -> Mark -> Form
markView { boardSize, gridSize, history } x y mark =
    let
        pos =
            moveInt
                ( x * boardSize // gridSize + (-boardSize + boardSize // gridSize) // 2
                , -y * boardSize // gridSize + (boardSize - boardSize // gridSize) // 2
                )

        color =
            if (List.head history) == Just ( x, y ) then
                activeMarkColor
            else
                markColor
    in
        case mark of
            EmptyTile ->
                pos <| group []

            TakenTile player ->
                case player of
                    PlayerOne ->
                        pos <| cross color boardSize

                    PlayerTwo ->
                        pos <| nought color boardSize


undoView : Model -> Html Msg
undoView { state } =
    case state of
        Started _ False ->
            button [ class "btn", onClick UndoHistory ] [ text "Undo" ]

        _ ->
            text ""


stylesView : Model -> Html Msg
stylesView { boardSize } =
    node "style" [] [ text (".screen { width: " ++ (toString boardSize) ++ "px; height: " ++ (toString boardSize) ++ "px;}") ]
