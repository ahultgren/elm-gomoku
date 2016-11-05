module Gomoku exposing (..)

import Html.App exposing (program)
import Html exposing (Html, text, div)
import Collage exposing (Form, collage, move, filled, rect, group)
import Color exposing (rgb)
import Element exposing (toHtml)


type alias Model =
    { width : Float
    , height : Float
    }


main : Program Never
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd x )
init =
    ( { width = 500, height = 500 }, Cmd.none )


update : a -> Model -> ( Model, Cmd a )
update _ model =
    ( model, Cmd.none )


gridSize : Float
gridSize =
    19


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


view : Model -> Html a
view { width, height } =
    div []
        [ toHtml <|
            collage
                (round width)
                (round height)
                [ move ( 0, 0 ) <|
                    group <|
                        grid width height gridSize
                ]
        ]
