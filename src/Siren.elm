module Siren exposing (Entity, decodeJson)

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Set exposing (Set)


-- type Property = String value | Int value | Float value | Boolean value


type alias Entity =
    { rels :
        Set String
    , classes :
        Set String
    , properties :
        Dict String String
    }


decodeJson : String -> Maybe Entity
decodeJson json =
    let
        result =
            decodeString entityDecoder json
    in
        case result of
            Ok e ->
                Just e

            _ ->
                Nothing


entityDecoder : Decoder Entity
entityDecoder =
    map3 Entity
        (setFieldDecoder "rel")
        (setFieldDecoder "class")
        (dictFieldDecoder "properties")


setFieldDecoder : String -> Decoder (Set String)
setFieldDecoder =
    optionalCollectionDecoder (list string) Set.fromList


dictFieldDecoder : String -> Decoder (Dict String String)
dictFieldDecoder =
    optionalCollectionDecoder (keyValuePairs string) dictFromPairs


optionalCollectionDecoder : Decoder (List a) -> (List a -> b) -> String -> Decoder b
optionalCollectionDecoder fieldDecoder constructor name =
    field name fieldDecoder
    |> maybe
    |> map withEmptyListDefault
    |> map constructor


dictFromPairs : List (String, String) -> Dict String String
dictFromPairs = List.foldr (\( k, v ) -> Dict.insert k v) Dict.empty


withEmptyListDefault : Maybe (List a) -> List a
withEmptyListDefault = Maybe.withDefault []
