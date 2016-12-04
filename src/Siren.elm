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


setFieldDecoder : String -> Decoder (Set String)
setFieldDecoder name =
    field name (list string)
        |> map Set.fromList
        |> withDefaultDecoder Set.empty


dictFieldDecoder : String -> Decoder (Dict String Value)
dictFieldDecoder name =
    field name propertiesDecoder
        |> withDefaultDecoder Dict.empty


linksFieldDecoder : String -> Decoder (Dict String String)
linksFieldDecoder name =
    field name linksDecoder
        |> withDefaultDecoder Dict.empty


linksDecoder : Decoder (Dict String String)
linksDecoder = map Dict.fromList (list linkDecoder)


linkDecoder : Decoder (String, String)
linkDecoder = 
    map2 (,)
        (map (Maybe.withDefault "unknown" << List.head) (field "rel" (list string)))
        (field "href" string)


withDefaultDecoder : a -> Decoder a -> Decoder a
withDefaultDecoder default decoder =
    oneOf [ decoder, succeed default ]


propertiesDecoder : Decoder (Dict String Value)
propertiesDecoder = dict propertyDecoder


propertyDecoder : Decoder Value
propertyDecoder =
    oneOf
        [ map StringValue string
        , map IntValue int
        , map FloatValue float
        , map BoolValue bool
        , null NullValue
        ]
