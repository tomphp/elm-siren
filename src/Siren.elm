module Siren exposing (Entity, decodeJson, Value(..))

import Dict exposing (Dict)
import Json.Decode exposing (..)
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


set : Decoder comparable -> Decoder (Set comparable)
set = list >> map Set.fromList 


setFieldDecoder : String -> Decoder (Set String)
setFieldDecoder = fieldWithDefault (set string) Set.empty


dictFieldDecoder : String -> Decoder (Dict String Value)
dictFieldDecoder = fieldWithDefault (dict valueDecoder) Dict.empty


linksFieldDecoder : String -> Decoder (Dict String String)
linksFieldDecoder = fieldWithDefault linksDecoder Dict.empty


fieldWithDefault : Decoder a -> a -> String -> Decoder a
fieldWithDefault decoder default name =
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
linksDecoder = map (List.concat >> Dict.fromList) <| list linkDecoder


linkDecoder : Decoder (List (String, String))
linkDecoder = 
    map expandTuples <|
        map2 (,)
            (field "rel" (list string))
            (field "href" string)


expandTuples : (List a, b) -> List (a, b)
expandTuples (keys, value) = List.map (\k -> (k, value)) keys
