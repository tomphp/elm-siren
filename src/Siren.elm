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
    map listToSet (maybe (field name (list string)))


listToSet : Maybe (List comparable) -> Set comparable
listToSet list =
    case list of
        Just l ->
            Set.fromList l

        Nothing ->
            Set.empty


dictFieldDecoder : String -> Decoder (Dict String String)
dictFieldDecoder name =
    map kvPairsToDict (maybe (field name (keyValuePairs string)))


kvPairsToDict : Maybe (List ( String, String )) -> Dict String String
kvPairsToDict mp =
    case mp of
        Just pairs ->
            List.foldr (\( k, v ) dict -> Dict.insert k v dict) Dict.empty pairs

        Nothing ->
            Dict.empty
