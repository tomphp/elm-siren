module Siren exposing (Entity, decodeJson)

import Set exposing (Set)
import Json.Decode exposing (Decoder , list , field, string , map , decodeString)   

-- type alias Rel = String
-- type alias Class = String
-- type Property = String value | Int value | Float value | Boolean value

type alias Entity =
  { rels: Set String
  -- , classes: Set Class
  -- , properties: Dict String Property
  -- , links: Dict String Link
  }

decodeJson : String -> Maybe Entity
decodeJson json = 
  let
      result = decodeString entityDecoder json
  in
      case result of
        Ok e -> Just e
        _    -> Nothing

entityDecoder : Decoder Entity
entityDecoder =
  field "rel" (list string)
  |> map Set.fromList
  |> map Entity

