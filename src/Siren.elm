module Siren exposing (Entity, decodeJson)

import Json.Decode exposing (..)
import Set exposing (Set)


-- type alias Rel = String
-- type alias Class = String
-- type Property = String value | Int value | Float value | Boolean value


type alias Entity =
    { rels :
        Set String
    , classes :
        Set String
        -- , properties: Dict String Property
        -- , links: Dict String Link
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
    map2 Entity
        (setFieldDecoder "rel")
        (setFieldDecoder "class")


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
