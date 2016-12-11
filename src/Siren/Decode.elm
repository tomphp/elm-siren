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

import Siren exposing (..)
import Set exposing (Set)
import Dict
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (set)


decodeJson : String -> Result String Entity
decodeJson =
    decodeString entity


entity : Decoder Entity
entity =
    map6 Entity
        (field "rel" stringSet |> withDefault Set.empty)
        (field "class" stringSet |> withDefault Set.empty)
        (field "properties" (dict value) |> withDefault Dict.empty)
        (field "links" (list link) |> withDefault [])
        (lazy (\() -> field "entities" (list embeddedEntity) |> withDefault []))
        (field "actions" (list action) |> withDefault [])


embeddedEntity : Decoder Entity
embeddedEntity =
    oneOf [ entityLink, entity ]


entityLink : Decoder Entity
entityLink =
    map5 EntityLink
        (field "rel" stringSet |> withDefault Set.empty)
        (field "class" stringSet |> withDefault Set.empty)
        (field "href" string)
        (field "type" string |> maybe)
        (field "title" string |> maybe)


link : Decoder Link
link =
    map2 Link
        (field "rel" stringSet)
        (field "href" string)


action : Decoder Action
action =
    map6 Action
        (field "name" string)
        (field "href" string)
        (field "class" stringSet |> withDefault Set.empty)
        (field "method" string |> withDefault "GET")
        (field "title" string |> maybe)
        (field "fields" (list actionField) |> withDefault [])


actionField : Decoder Field
actionField =
    map5 Field
        (field "name" string)
        (succeed Set.empty)
        (succeed "text")
        (succeed Nothing)
        (succeed Nothing)


value : Decoder Siren.Value
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


withDefault : a -> Decoder a -> Decoder a
withDefault default decoder =
    oneOf [ decoder, succeed default ]
