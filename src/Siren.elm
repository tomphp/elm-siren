module Siren exposing (Entity, decodeJson, Value(..))

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (set)
import Set exposing (Set)


type Value
    = StringValue String
    | IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | NullValue


type alias Entity =
    { rels :
        Set String
    , classes :
        Set String
    , properties :
        Dict String Value
    , links :
        Dict String String
    }


decodeJson : String -> Result String Entity
decodeJson json =
    decodeString entityDecoder json


entityDecoder : Decoder Entity
entityDecoder =
    map4 Entity
        (setFieldDecoder "rel")
        (setFieldDecoder "class")
        (dictFieldDecoder "properties")
        (linksFieldDecoder "links")


setFieldDecoder : String -> Decoder (Set String)
setFieldDecoder =
    decodeWithDefault (set string) Set.empty


dictFieldDecoder : String -> Decoder (Dict String Value)
dictFieldDecoder =
    decodeWithDefault (dict valueDecoder) Dict.empty


linksFieldDecoder : String -> Decoder (Dict String String)
linksFieldDecoder =
    decodeWithDefault linksDecoder Dict.empty


decodeWithDefault : Decoder a -> a -> String -> Decoder a
decodeWithDefault decoder default name =
    field name decoder |> withDefault default


withDefault : a -> Decoder a -> Decoder a
withDefault default decoder =
    oneOf [ decoder, succeed default ]


valueDecoder : Decoder Value
valueDecoder =
    oneOf
        [ map StringValue string
        , map IntValue int
        , map FloatValue float
        , map BoolValue bool
        , null NullValue
        ]


linksDecoder : Decoder (Dict String String)
linksDecoder =
    linkDecoder
        |> list
        |> map (List.concat >> Dict.fromList)


linkDecoder : Decoder (List ( String, String ))
linkDecoder =
    map expandTuples <|
        map2 (,)
            (field "rel" (list string))
            (field "href" string)


expandTuples : ( List a, b ) -> List ( a, b )
expandTuples ( keys, value ) =
    List.map (\k -> ( k, value )) keys
