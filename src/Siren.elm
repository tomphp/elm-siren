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
setFieldDecoder name =
    field name (list string)
    |> maybe
    |> map withEmptyListDefault
    |> map Set.fromList


dictFieldDecoder : String -> Decoder (Dict String String)
dictFieldDecoder name =
    field name (keyValuePairs string)
    |> maybe
    |> map withEmptyListDefault
    |> map dictFromPairs


dictFromPairs : List (String, String) -> Dict String String
dictFromPairs = List.foldr (\( k, v ) -> Dict.insert k v) Dict.empty


withEmptyListDefault : Maybe (List a) -> List a
withEmptyListDefault = Maybe.withDefault []
