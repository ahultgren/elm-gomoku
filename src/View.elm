module View exposing (..)

import Dict
import Json.Decode as Decode exposing (Decoder)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class)
import Html.Events exposing (on, onClick)
import Collage exposing (Form, collage, move, filled, outlined, rect, oval, group, polygon, defaultLine)
import Color exposing (rgb, rgba)
import Element exposing (toHtml)
import Types
    exposing
        ( Model
        , Marks
        , Row
        , Msg(TileClick, UndoHistory)
        , Mark(EmptyTile, TakenTile)
        , Player(PlayerOne, PlayerTwo)
        , Coords
        )


decodeClickLocation : Decoder Coords
decodeClickLocation =
    Decode.object2 (,)
        (Decode.object2 (-)
            (Decode.at [ "pageX" ] Decode.int)
            (Decode.at [ "target", "offsetLeft" ] Decode.int)
        )
        (Decode.object2 (-)
            (Decode.at [ "pageY" ] Decode.int)
            (Decode.at [ "target", "offsetTop" ] Decode.int)
        )


coordsToTile : Int -> Int -> Coords -> Coords
coordsToTile boardSize count ( x, y ) =
    ( count * x // boardSize, count * y // boardSize )


view : Model -> Html Msg
view model =
    div []
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
        , wonView model
        , button [ onClick UndoHistory ] [ text "Undo" ]
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
playerView { currentPlayer } =
    text ("Turn: " ++ playerToString currentPlayer)


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


nought : Int -> Form
nought boardSize =
    let
        diameter =
            toFloat <| boardSize // 50
    in
        outlined { defaultLine | color = (rgb 3 3 3), width = 2 } <| oval diameter diameter


cross : Int -> Form
cross boardSize =
    let
        corner =
            toFloat <| boardSize // 100
    in
        outlined { defaultLine | color = (rgb 3 3 3), width = 2 } <|
            polygon [ ( 0, 0 ), ( -corner, -corner ), ( corner, corner ), ( 0, 0 ), ( corner, -corner ), ( -corner, corner ) ]


markView : Model -> Int -> Int -> Mark -> Form
markView { boardSize, gridSize } x y mark =
    let
        pos =
            moveInt
                ( x * boardSize // gridSize + (-boardSize + boardSize // gridSize) // 2
                , -y * boardSize // gridSize + (boardSize - boardSize // gridSize) // 2
                )
    in
        case mark of
            EmptyTile ->
                pos <| group []

            TakenTile player ->
                case player of
                    PlayerOne ->
                        pos <| cross boardSize

                    PlayerTwo ->
                        pos <| nought boardSize


wonView : Model -> Html Msg
wonView { hasWon } =
    case hasWon of
        Nothing ->
            div [] []

        Just player ->
            div [ class "winner" ] [ text <| (playerToString player) ++ " won!" ]
