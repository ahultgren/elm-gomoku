module Decoder exposing (..)

import Json.Decode as Json exposing (Decoder, string, int, float, dict, list, bool, map, value, decodeValue, decodeString)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
customDecoder decoder toResult =
    Json.andThen
        (\a ->
            case toResult a of
                Ok b ->
                    Json.succeed b

                Err err ->
                    Json.fail err
        )
        decoder


maybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybe name decoder =
    optional name (map Just decoder) Nothing


lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
    customDecoder value
        (\js -> decodeValue (thunk ()) js)


type EnumType
    = EnumTypeMove
    | EnumTypeStart
    | EnumTypeDisconnected


type EnumPlayer
    = EnumPlayerPlayerOne
    | EnumPlayerPlayerTwo


type alias Command =
    { type_ : EnumType
    }


type alias Coords =
    { x : Int
    , y : Int
    }


type alias Move =
    { coords : Coords
    }


type alias Start =
    { player : EnumPlayer
    }


decodeEnumType : Decoder EnumType
decodeEnumType =
    let
        decodeToType string =
            case string of
                "Move" ->
                    Result.Ok EnumTypeMove

                "Start" ->
                    Result.Ok EnumTypeStart

                "Disconnected" ->
                    Result.Ok EnumTypeDisconnected

                _ ->
                    Result.Err ("Invalid value for EnumType. Value: " ++ string)
    in
        customDecoder string decodeToType


decodeEnumPlayer : Decoder EnumPlayer
decodeEnumPlayer =
    let
        decodeToType string =
            case string of
                "PlayerOne" ->
                    Result.Ok EnumPlayerPlayerOne

                "PlayerTwo" ->
                    Result.Ok EnumPlayerPlayerTwo

                _ ->
                    Result.Err ("Invalid value for EnumPlayer. Value: " ++ string)
    in
        customDecoder string decodeToType


decodeCommand : Decoder Command
decodeCommand =
    decode Command
        |> required "type" decodeEnumType


decodeCoords : Decoder Coords
decodeCoords =
    decode Coords
        |> required "x" int
        |> required "y" int


decodeMove : Decoder Move
decodeMove =
    decode Move
        |> required "coords" decodeCoords


decodeStart : Decoder Start
decodeStart =
    decode Start
        |> required "player" decodeEnumPlayer
