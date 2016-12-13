module Siren.Decode
    exposing
        ( action
        , actionField
        , decodeJson
        , entity
        , entityLink
        , link
        , value
        )

import Dict
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (set)
import Set exposing (Set)
import Siren.Entity exposing (..)
import Siren.Value exposing (Value(..))


decodeJson : String -> Result String Entity
decodeJson =
    decodeString entity


entity : Decoder Entity
entity =
    map6 Entity
        (field "rel" stringSet |> default Set.empty)
        (field "class" stringSet |> default Set.empty)
        (field "properties" (dict value) |> default Dict.empty)
        (field "links" (list link) |> default [])
        (lazy (\() -> field "entities" (list embeddedEntity) |> default []))
        (field "actions" (list action) |> default [])


embeddedEntity : Decoder EmbeddedEntity
embeddedEntity =
    oneOf [ entityLink, map EmbeddedRepresentation entity ]


entityLink : Decoder EmbeddedEntity
entityLink =
    map EmbeddedLink <|
        map5 EntityLink
            (field "rel" stringSet |> default Set.empty)
            (field "class" stringSet |> default Set.empty)
            (field "href" string)
            (field "type" string |> maybe)
            (field "title" string |> maybe)


link : Decoder Link
link =
    map5 Link
        (field "rel" stringSet)
        (field "class" stringSet |> default Set.empty)
        (field "href" string)
        (field "title" string |> maybe)
        (field "type" string |> maybe)


action : Decoder Action
action =
    map6 Action
        (field "name" string)
        (field "href" string)
        (field "class" stringSet |> default Set.empty)
        (field "method" string |> default "GET")
        (field "title" string |> maybe)
        (field "fields" (list actionField) |> default [])


actionField : Decoder Field
actionField =
    map5 Field
        (field "name" string)
        (field "class" stringSet |> default Set.empty)
        (field "type" string |> default "text")
        (field "value" string |> maybe)
        (field "title" string |> maybe)


value : Decoder Siren.Value.Value
value =
    oneOf
        [ map StringValue string
        , map IntValue int
        , map FloatValue float
        , map BoolValue bool
        , null NullValue
        ]


stringSet : Decoder (Set String)
stringSet =
    set string


default : a -> Decoder a -> Decoder a
default default decoder =
    oneOf [ decoder, succeed default ]
