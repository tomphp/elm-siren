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
decodeJson json =
    decodeString entityDecoder json


entityDecoder : Decoder Entity
entityDecoder =
    map6 Entity
        (setFieldDecoder "rel")
        (setFieldDecoder "class")
        (dictFieldDecoder "properties")
        (linksFieldDecoder "links")
        (lazy (\_ -> entitiesFieldDecoder "entities"))
        (actionsFieldDecoder "actions")


entityLinkDecoder : Decoder Entity
entityLinkDecoder =
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
        (list <| oneOf [ entityLinkDecoder, entityDecoder ])


linksFieldDecoder : String -> Decoder (Dict String String)
linksFieldDecoder =
    decodeFieldWithDefault Dict.empty linksDecoder


actionsFieldDecoder : String -> Decoder (Dict String Action)
actionsFieldDecoder =
    decodeFieldWithDefault Dict.empty actionsDecoder


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


linksDecoder : Decoder (Dict String String)
linksDecoder =
    linkDecoder
        |> list
        |> map (List.concat >> Dict.fromList)


linkDecoder : Decoder (List ( String, String ))
linkDecoder =
    map expandTuples <|
        map2 (,)
            (field "rel" (list string))
            (field "href" string)


actionsDecoder : Decoder (Dict String Action)
actionsDecoder =
    map Dict.fromList <| list actionTupleDecoder


actionTupleDecoder : Decoder ( String, Action )
actionTupleDecoder =
    map2 (,)
        (field "name" string)
        actionDecoder


actionDecoder : Decoder Action
actionDecoder =
    map5 Action
        (field "href" string)
        (setFieldDecoder "class")
        (oneOf [ field "method" string, succeed "GET" ])
        (maybe <| field "title" string)
        (succeed [])


expandTuples : ( List a, b ) -> List ( a, b )
expandTuples ( keys, value ) =
    List.map (\k -> ( k, value )) keys
