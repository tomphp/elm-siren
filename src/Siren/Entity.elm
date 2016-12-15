module Siren.Entity
    exposing
        ( linksWithRel
        , linksWithClass
        , embeddedEntitiesWithClass
        , embeddedEntities
        , firstLinkWithRel
        , propertyString
        , property
        , Action
        , EmbeddedEntity(..)
        , Entity
        , EntityLink
        , Field
        , Link
        , Property
        )

import Dict exposing (Dict)
import Set exposing (Set)
import Siren.Value exposing (Value(..))


type alias Entity =
    { rels : Set String
    , classes : Set String
    , properties : Dict String Property
    , links : List Link
    , entities : List EmbeddedEntity
    , actions : List Action
    }


type alias EntityLink =
    { rels : Set String
    , classes : Set String
    , href : String
    , mediaType : Maybe String
    , title : Maybe String
    }


type EmbeddedEntity
    = EmbeddedRepresentation Entity
    | EmbeddedLink EntityLink


type alias Action =
    { name : String
    , href : String
    , classes : Set String
    , method : String
    , title : Maybe String
    , fields : List Field
    }


type alias Field =
    { name : String
    , classes : Set String
    , fieldType : String
    , value : Maybe String
    , title : Maybe String
    }


type alias Link =
    { rels : Set String
    , classes : Set String
    , href : String
    , title : Maybe String
    , mediaType : Maybe String
    }


type alias Property =
    Value


linksWithRel : String -> Entity -> List Link
linksWithRel rel =
    .links >> List.filter (.rels >> Set.member rel)


firstLinkWithRel : String -> Entity -> Maybe Link
firstLinkWithRel name =
    linksWithRel name >> List.head


linksWithClass : String -> Entity -> List Link
linksWithClass class =
    .links >> List.filter (.classes >> Set.member class)


property : String -> Entity -> Maybe Value
property name =
    .properties >> Dict.get name


propertyString : String -> Entity -> Maybe String
propertyString name =
    property name >> Maybe.map Siren.Value.toString


embeddedEntitiesWithClass : String -> Entity -> List Entity
embeddedEntitiesWithClass class =
    embeddedEntities >> List.filter (hasClass class)


embeddedEntities : Entity -> List Entity
embeddedEntities =
    .entities
        >> List.map embeddedEntity
        >> List.filterMap identity



-- private


hasClass : String -> { a | classes : Set String } -> Bool
hasClass class =
    .classes >> Set.member class


embeddedEntity : EmbeddedEntity -> Maybe Entity
embeddedEntity entity =
    case entity of
        EmbeddedRepresentation record ->
            Just record

        EmbeddedLink record ->
            Nothing
