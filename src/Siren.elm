module Siren
    exposing
        ( decodeJson
        , Value(..)
        , Rel
        , Rels
        , Class
        , Classes
        , Property
        , Properties
        , Href
        , Links
        , Entity(..)
        , Entities
        , Action
        , Actions
        )

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (set)
import Set exposing (Set)


type Value
    = StringValue String
    | IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | NullValue


type alias Rel =
    String


type alias Rels =
    Set String


type alias Class =
    String


type alias Classes =
    Set String


type alias Property =
    Value


type alias Properties =
    Dict String Value


type alias Href =
    String


type alias Links =
    Dict String Href


type alias Entities =
    List Entity


type alias Actions =
    Dict String Action


type alias Action =
    { href : Href
    , classes : Classes
    , method : String
    , title : Maybe String
    , fields : List String
    }


type Entity
    = Entity Rels Classes Properties Links Entities Actions
    | EntityLink Rels Classes Href (Maybe String) (Maybe String)


decodeJson : String -> Result String Entity
decodeJson =
    decodeString entities


entities : Decoder Entity
entities =
    map6 Entity
        (setFieldDecoder "rel")
        (setFieldDecoder "class")
        (dictFieldDecoder "properties")
        (linksFieldDecoder "links")
        (lazy (\_ -> entitiesFieldDecoder "entities"))
        (actionsFieldDecoder "actions")


entityLink : Decoder Entity
entityLink =
    map5 EntityLink
        (setFieldDecoder "rel")
        (setFieldDecoder "class")
        (field "href" string)
        (maybe <| field "type" string)
        (maybe <| field "title" string)


setFieldDecoder : String -> Decoder (Set String)
setFieldDecoder =
    decodeFieldWithDefault Set.empty <| (set string)


dictFieldDecoder : String -> Decoder (Dict String Value)
dictFieldDecoder =
    decodeFieldWithDefault Dict.empty <| dict valueDecoder


entitiesFieldDecoder : String -> Decoder (List Entity)
entitiesFieldDecoder =
    decodeFieldWithDefault
        []
        (list <| oneOf [ entityLink, entities ])


linksFieldDecoder : String -> Decoder (Dict String String)
linksFieldDecoder =
    decodeFieldWithDefault Dict.empty links


actionsFieldDecoder : String -> Decoder (Dict String Action)
actionsFieldDecoder =
    decodeFieldWithDefault Dict.empty actions


decodeFieldWithDefault : a -> Decoder a -> String -> Decoder a
decodeFieldWithDefault default decoder name =
    field name decoder |> withDefault default


withDefault : a -> Decoder a -> Decoder a
withDefault default decoder =
    oneOf [ decoder, succeed default ]


valueDecoder : Decoder Value
valueDecoder =
    oneOf
        [ map StringValue string
        , map IntValue int
        , map FloatValue float
        , map BoolValue bool
        , null NullValue
        ]


links : Decoder (Dict String String)
links =
    linkDecoder
        |> list
        |> map (List.concat >> Dict.fromList)


linkDecoder : Decoder (List ( String, String ))
linkDecoder =
    map denormaliseListTuple <|
        map2 (,)
            (field "rel" (list string))
            (field "href" string)


actions : Decoder (Dict String Action)
actions =
    map Dict.fromList <| list actionTupleDecoder


actionTupleDecoder : Decoder ( String, Action )
actionTupleDecoder =
    map2 (,)
        (field "name" string)
        action


action : Decoder Action
action =
    map5 Action
        (field "href" string)
        (setFieldDecoder "class")
        (oneOf [ field "method" string, succeed "GET" ])
        (maybe <| field "title" string)
        (succeed [])


denormaliseListTuple : ( List a, b ) -> List ( a, b )
denormaliseListTuple ( keys, value ) =
    List.map (\k -> ( k, value )) keys
