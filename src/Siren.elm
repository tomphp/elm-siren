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
setFieldDecoder =
    optionalCollectionDecoder (list string) Set.fromList


dictFieldDecoder : String -> Decoder (Dict String Property)
dictFieldDecoder =
    optionalCollectionDecoder propertyDecoder dictFromPairs

propertyDecoder : Decoder (List (String, Property))
propertyDecoder = map convertProperties (keyValuePairs string)

convertProperties : List (String, String) -> List (String, Property)
convertProperties = List.map stringToProperty

stringToProperty : (String, String) -> (String, Property)
stringToProperty (k, v) = (k, StringProperty v)

optionalCollectionDecoder : Decoder (List a) -> (List a -> b) -> String -> Decoder b
optionalCollectionDecoder fieldDecoder constructor name =
    field name fieldDecoder
    |> maybe
    |> map withEmptyListDefault
    |> map constructor


dictFromPairs : List (String, Property) -> Dict String Property
dictFromPairs = List.foldr insertIntoDict Dict.empty


insertIntoDict : (comparable, Property) -> Dict comparable Property -> Dict comparable Property
insertIntoDict (k, v) = Dict.insert k v


withEmptyListDefault : Maybe (List a) -> List a
withEmptyListDefault = Maybe.withDefault []
