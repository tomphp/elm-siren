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


type alias Field =
    { name : String
    , class : List String
    , fieldType : String
    , value : Maybe String
    , title : Maybe String
    }


type Entity
    = Entity Rels Classes Properties Links Entities Actions
    | EntityLink Rels Classes Href (Maybe String) (Maybe String)


decodeJson : String -> Result String Entity
decodeJson =
    decodeString entity


entity : Decoder Entity
entity =
    map6 Entity
        (fieldOrDefault "rel" Set.empty stringSet)
        (fieldOrDefault "class" Set.empty stringSet)
        (fieldOrDefault "properties" Dict.empty values)
        (fieldOrDefault "links" Dict.empty links)
        (lazy (\_ -> fieldOrDefault "entities" [] (list embeddedEntity)))
        (fieldOrDefault "actions" Dict.empty actions)


embeddedEntity : Decoder Entity
embeddedEntity =
    oneOf [ entityLink, entity ]


entityLink : Decoder Entity
entityLink =
    map5 EntityLink
        (fieldOrDefault "rel" Set.empty stringSet)
        (fieldOrDefault "class" Set.empty stringSet)
        (field "href" string)
        (maybe <| field "type" string)
        (maybe <| field "title" string)


stringSet : Decoder (Set String)
stringSet =
    set string


fieldOrDefault : String -> a -> Decoder a -> Decoder a
fieldOrDefault name default decoder =
    withDefault default <| field name decoder


withDefault : a -> Decoder a -> Decoder a
withDefault default decoder =
    oneOf [ decoder, succeed default ]


values : Decoder (Dict String Value)
values =
    dict <|
        oneOf
            [ map StringValue string
            , map IntValue int
            , map FloatValue float
            , map BoolValue bool
            , null NullValue
            ]


links : Decoder (Dict String String)
links =
    link
        |> list
        |> map (List.concat >> Dict.fromList)


link : Decoder (List ( String, String ))
link =
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
        (fieldOrDefault "class" Set.empty stringSet)
        (oneOf [ field "method" string, succeed "GET" ])
        (maybe <| field "title" string)
        (succeed [])


denormaliseListTuple : ( List a, b ) -> List ( a, b )
denormaliseListTuple ( keys, value ) =
    List.map (\k -> ( k, value )) keys
