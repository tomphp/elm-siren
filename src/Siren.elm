module Siren exposing (Entity, decodeJson, Property(..))

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Set exposing (Set)


type Property = StringProperty String | IntProperty Int


type alias Entity =
    { rels :
        Set String
    , classes :
        Set String
    , properties :
        Dict String Property
    }


decodeJson : String -> Maybe Entity
decodeJson json =
    Result.toMaybe (decodeString entityDecoder json)


entityDecoder : Decoder Entity
entityDecoder =
    map3 Entity
        (setFieldDecoder "rel")
        (setFieldDecoder "class")
        (dictFieldDecoder "properties")


setFieldDecoder : String -> Decoder (Set String)
setFieldDecoder name =
    oneOf [ optionalCollectionDecoder (list string) Set.fromList name, succeed Set.empty ]


dictFieldDecoder : String -> Decoder (Dict String Property)
dictFieldDecoder name =
    oneOf [ optionalCollectionDecoder propertiesDecoder dictFromPairs name, succeed Dict.empty ]


optionalCollectionDecoder : Decoder (List a) -> (List a -> b) -> String -> Decoder b
optionalCollectionDecoder fieldDecoder constructor name =
    field name fieldDecoder
    |> map constructor


propertiesDecoder : Decoder (List (String, Property))
propertiesDecoder = keyValuePairs propertyDecoder


propertyDecoder : Decoder Property
propertyDecoder =
    oneOf [ map StringProperty string
          , map IntProperty int
          ]


dictFromPairs : List (String, Property) -> Dict String Property
dictFromPairs = List.foldr insertIntoDict Dict.empty


insertIntoDict : (comparable, Property) -> Dict comparable Property -> Dict comparable Property
insertIntoDict (k, v) = Dict.insert k v
