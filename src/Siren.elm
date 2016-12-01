module Siren exposing (Entity, decodeJson)

import Set exposing (Set)
import Json.Decode exposing (Decoder, list, field, string, map, maybe, decodeString)


-- type alias Rel = String
-- type alias Class = String
-- type Property = String value | Int value | Float value | Boolean value


type alias Entity =
    { rels :
        Set String
        -- , classes: Set Class
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
    maybe (field "rel" (list string))
        |> map listToSet
        |> map Entity


listToSet : Maybe (List comparable) -> Set comparable
listToSet list =
    case list of
        Just l ->
            Set.fromList l

        Nothing ->
            Set.empty
