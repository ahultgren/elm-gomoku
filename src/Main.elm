module Gomoku exposing (..)

import Dict exposing (Dict)
import Maybe.Extra exposing (join)
import Html exposing (Html, text, div)
import Html.App exposing (program)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import Collage exposing (Form, collage, move, filled, rect, oval, group)
import Color exposing (rgb, rgba)
import Element exposing (toHtml)


type alias Model =
    { width : Float
    , height : Float
    , gridSize : Int
    , marks : Marks
    , currentPlayer : Player
    }


type alias Marks =
    Dict Int Row


type alias Row =
    Dict Int Mark


type Msg
    = TileClick ( Int, Int )


type Mark
    = EmptyTile
    | TakenTile Player


type Player
    = PlayerOne
    | PlayerTwo


main : Program Never
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions x =
    Sub.none


generateEmptyBoard : Int -> Marks
generateEmptyBoard count =
    Dict.fromList <|
        List.indexedMap (,) <|
            List.repeat count <|
                Dict.fromList <|
                    List.indexedMap (,) <|
                        List.repeat count EmptyTile


initGridSize : Int
initGridSize =
    19


init : ( Model, Cmd x )
init =
    ( { width = 500
      , height = 500
      , gridSize = initGridSize
      , marks = generateEmptyBoard initGridSize
      , currentPlayer = PlayerOne
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TileClick ( x, y ) ->
            if x < 0 || y < 0 || x > model.gridSize || y > model.gridSize then
                ( model, Cmd.none )
            else
                ( { model
                    | marks = updateMarks ( x, y ) model.currentPlayer model.marks
                    , currentPlayer = updatePlayer model.marks ( x, y ) model.currentPlayer
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


decodeClickLocation : Decoder ( Int, Int )
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


coordsToTile : Float -> Float -> Float -> ( Int, Int ) -> ( Int, Int )
coordsToTile width height count ( x, y ) =
    ( floor ((toFloat x) / width * count), floor ((toFloat y) / height * count) )


view : Model -> Html Msg
view model =
    div [ on "click" (Decode.map (coordsToTile model.width model.height (toFloat model.gridSize) >> TileClick) decodeClickLocation) ]
        [ toHtml <|
            collage
                (round model.width)
                (round model.height)
                [ group <|
                    grid model.width model.height (toFloat model.gridSize)
                , marksView model
                ]
        , playerView model
        ]


playerView : Model -> Html Msg
playerView { currentPlayer } =
    case currentPlayer of
        PlayerOne ->
            text "Turn: Black"

        PlayerTwo ->
            text "Turn: White"


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
