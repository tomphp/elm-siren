module Siren.Entity
    exposing
        ( linksWithRel
        , linksWithClass
        , Action
        , Actions
        , Class
        , Classes
        , EmbeddedEntity(..)
        , Entities
        , Entity
        , EntityLink
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


type alias Entity =
    { rels : Rels
    , classes : Classes
    , properties : Properties
    , links : Links
    , entities : Entities
    , actions : Actions
    }


type alias EntityLink =
    { rels : Rels
    , classes : Classes
    , href : Href
    , mediaType : Maybe String
    , title : Maybe String
    }


type EmbeddedEntity
    = EmbeddedRepresentation Entity
    | EmbeddedLink EntityLink


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
    List EmbeddedEntity


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
    entity.links |> List.filter (.rels >> Set.member rel)


linksWithClass : String -> Entity -> Links
linksWithClass class entity =
    entity.links |> List.filter (.classes >> Set.member class)
