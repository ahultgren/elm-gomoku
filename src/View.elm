module View exposing (..)

import Dict
import Json.Decode as Decode exposing (Decoder)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class)
import Html.Events exposing (on, onClick)
import Collage exposing (Form, collage, move, filled, rect, oval, group)
import Color exposing (rgb, rgba)
import Element exposing (toHtml)
import Types
    exposing
        ( Model
        , Marks
        , Row
        , Msg(TileClick, CheckWinCondition, UndoHistory)
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


coordsToTile : Float -> Float -> Float -> Coords -> Coords
coordsToTile width height count ( x, y ) =
    ( floor ((toFloat x) / width * count), floor ((toFloat y) / height * count) )


view : Model -> Html Msg
view model =
    div []
        [ div [ on "click" (Decode.map (coordsToTile model.width model.height (toFloat model.gridSize) >> TileClick) decodeClickLocation) ]
            [ toHtml <|
                collage
                    (round model.width)
                    (round model.height)
                    [ group <|
                        grid model.width model.height (toFloat model.gridSize)
                    , marksView model
                    ]
            ]
        , playerView model
        , wonView model
        , button [ onClick UndoHistory ] [ text "Undo" ]
        ]


grid : Float -> Float -> Float -> List Form
grid width height count =
    let
        verticalLines =
            List.repeat (round count + 1) ""
                |> List.indexedMap
                    (\i _ ->
                        move ( width / count * (toFloat i) - width / 2, 0 ) <|
                            filled (rgb 0 0 0) <|
                                rect 1 height
                    )

        horizontalLines =
            List.repeat (round count + 1) ""
                |> List.indexedMap
                    (\i _ ->
                        move ( 0, height / count * (toFloat i) - height / 2 ) <|
                            filled (rgb 0 0 0) <|
                                rect width 1
                    )
    in
        List.append verticalLines horizontalLines


playerView : Model -> Html Msg
playerView { currentPlayer } =
    text ("Turn: " ++ playerToString currentPlayer)


playerToString : Player -> String
playerToString player =
    case player of
        PlayerOne ->
            "Black"

        PlayerTwo ->
            "White"


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


markView : Model -> Int -> Int -> Mark -> Form
markView { width, height, gridSize } x y mark =
    let
        cellWidth =
            (width / (toFloat gridSize))

        cellHeight =
            (height / (toFloat gridSize))

        pos =
            move
                ( cellWidth * (toFloat x) - width / 2 + cellWidth / 2
                , cellHeight * (toFloat -y) + height / 2 - cellHeight / 2
                )

        shape =
            oval 10 10
    in
        case mark of
            EmptyTile ->
                pos <| filled (rgba 0 0 0 0) shape

            TakenTile player ->
                case player of
                    PlayerOne ->
                        pos <| filled (rgb 0 0 0) shape

                    PlayerTwo ->
                        pos <| filled (rgb 255 255 255) shape


wonView : Model -> Html Msg
wonView { hasWon } =
    case hasWon of
        Nothing ->
            div [] []

        Just player ->
            div [ class "winner" ] [ text <| (playerToString player) ++ " won!" ]
