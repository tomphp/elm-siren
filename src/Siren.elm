module Siren
    exposing
        ( linksWithRel
        , linksWithClass
        , Action
        , Actions
        , Class
        , Classes
        , Entities
        , Entity(..)
        , Field
        , Href
        , Link
        , Links
        , Properties
        , Property
        , Rel
        , Rels
        , Value(..)
        )

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (set)
import Set exposing (Set)


type Entity
    = Entity Rels Classes Properties Links Entities Actions
    | EntityLink Rels Classes Href (Maybe String) (Maybe String)


type alias Action =
    { name : String
    , href : Href
    , classes : Classes
    , method : String
    , title : Maybe String
    , fields : List Field
    }


type alias Field =
    { name : String
    , classes : Classes
    , fieldType : String
    , value : Maybe String
    , title : Maybe String
    }


type alias Link =
    { rels : Rels
    , classes : Classes
    , href : Href
    , title : Maybe String
    , mediaType : Maybe String
    }


type Value
    = StringValue String
    | IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | NullValue


type alias Links =
    List Link


type alias Rels =
    Set String


type alias Classes =
    Set Class


type alias Properties =
    Dict String Value


type alias Entities =
    List Entity


type alias Actions =
    List Action


type alias Rel =
    String


type alias Class =
    String


type alias Property =
    Value


type alias Href =
    String


linksWithRel : String -> Entity -> Links
linksWithRel rel entity =
    links entity |> List.filter (.rels >> Set.member rel)


linksWithClass : String -> Entity -> Links
linksWithClass class entity =
    links entity |> List.filter (.classes >> Set.member class)


links : Entity -> Links
links entity =
    case entity of
        Entity _ _ _ links _ _ ->
            links

        EntityLink _ _ _ _ _ ->
            []
