module Siren exposing (Entity, decodeJson, Property(..))

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Set exposing (Set)


type Property
    = StringProperty String
    | IntProperty Int
    | FloatProperty Float
    | BoolProperty Bool
    | NullProperty


type alias Entity =
    { rels :
        Set String
    , classes :
        Set String
    , properties :
        Dict String Property
    }


decodeJson : String -> Result String Entity
decodeJson json =
    decodeString entityDecoder json


entityDecoder : Decoder Entity
entityDecoder =
    map3 Entity
        (setFieldDecoder "rel")
        (setFieldDecoder "class")
        (dictFieldDecoder "properties")


setFieldDecoder : String -> Decoder (Set String)
setFieldDecoder name =
    field name (list string)
        |> map Set.fromList
        |> withDefaultDecoder Set.empty


dictFieldDecoder : String -> Decoder (Dict String Property)
dictFieldDecoder name =
    field name propertiesDecoder
        |> withDefaultDecoder Dict.empty


withDefaultDecoder : a -> Decoder a -> Decoder a
withDefaultDecoder default decoder =
    oneOf [ decoder, succeed default ]


propertiesDecoder : Decoder (Dict String Property)
propertiesDecoder =
    dict propertyDecoder


propertyDecoder : Decoder Property
propertyDecoder =
    oneOf
        [ map StringProperty string
        , map IntProperty int
        , map FloatProperty float
        , map BoolProperty bool
        , null NullProperty
        ]
