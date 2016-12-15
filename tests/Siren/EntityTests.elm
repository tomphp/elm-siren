module Siren.EntityTests exposing (..)

import Dict
import Expect
import Set
import Siren.Entity exposing (..)
import Test exposing (..)
import Siren.Value exposing (Value(..))


all : Test
all =
    describe "Siren.Entity"
        [ linksWithRelTests
        , firstLinkWithRelTests
        , linksWithClassTests
        , propertyTests
        , propertyStringTests
        , embeddedEntitiesWithClassTests
        , embeddedEntitiesTests
        ]


linksWithRelTests : Test
linksWithRelTests =
    let
        link1 =
            linkWithRels [ "rel1" ] "http://example.com/1"

        link2 =
            linkWithRels [ "rel1", "rel2" ] "http://example.com/2"

        link3 =
            linkWithRels [ "rel2" ] "http://example.com/3"

        entity =
            { rels = Set.empty
            , classes = Set.empty
            , properties = Dict.empty
            , links = [ link1, link2, link3 ]
            , entities = []
            , actions = []
            }
    in
        describe "linksWithClass"
            [ test "It returns the matching links" <|
                \() ->
                    Expect.equal [ link1, link2 ] (linksWithRel "rel1" entity)
            ]


firstLinkWithRelTests : Test
firstLinkWithRelTests =
    let
        link =
            linkWithRels [ "the-rel" ] "http://example.com/1"

        entity =
            { rels = Set.empty
            , classes = Set.empty
            , properties = Dict.empty
            , links = [ link ]
            , entities = []
            , actions = []
            }
    in
        describe "firstLinkWithClass"
            [ test "It returns nothing if the rel is not found" <|
                \() ->
                    Expect.equal Nothing (firstLinkWithRel "unknown" entity)
            , test "It returns just the matching link" <|
                \() ->
                    Expect.equal (Just link) (firstLinkWithRel "the-rel" entity)
            ]


linksWithClassTests : Test
linksWithClassTests =
    let
        link1 =
            linkWithClasses [ "class1" ] "http://example.com/1"

        link2 =
            linkWithClasses [ "class1", "class2" ] "http://example.com/2"

        link3 =
            linkWithClasses [ "class2" ] "http://example.com/3"

        entity =
            { rels = Set.empty
            , classes = Set.empty
            , properties = Dict.empty
            , links = [ link1, link2, link3 ]
            , entities = []
            , actions = []
            }
    in
        describe "linksWithClass"
            [ test "It returns the matching links" <|
                \() ->
                    Expect.equal [ link1, link2 ] (linksWithClass "class1" entity)
            ]


propertyTests : Test
propertyTests =
    let
        entity =
            { rels = Set.empty
            , classes = Set.empty
            , properties = Dict.singleton "the-property" <| StringValue "example"
            , links = []
            , entities = []
            , actions = []
            }
    in
        describe "property"
            [ test "It returns nothing if the property is not found" <|
                \() ->
                    Expect.equal Nothing (property "unknown" entity)
            , test "It returns just the matching value" <|
                \() ->
                    Expect.equal (Just <| StringValue "example") (property "the-property" entity)
            ]


propertyStringTests : Test
propertyStringTests =
    let
        entity =
            { rels = Set.empty
            , classes = Set.empty
            , properties = Dict.singleton "the-property" <| IntValue 99
            , links = []
            , entities = []
            , actions = []
            }
    in
        describe "propertyString"
            [ test "It returns nothing if the property is not found" <|
                \() ->
                    Expect.equal Nothing (propertyString "unknown" entity)
            , test "It returns just the matching value" <|
                \() ->
                    Expect.equal (Just "99") (propertyString "the-property" entity)
            ]


embeddedEntitiesWithClassTests : Test
embeddedEntitiesWithClassTests =
    let
        entity1 =
            entityWithClasses [ "class1" ] "entity one"

        entity2 =
            entityWithClasses [ "class1", "class2" ] "entity two"

        entity3 =
            entityWithClasses [ "class2" ] "entity three"

        entity =
            { rels = Set.empty
            , classes = Set.empty
            , properties = Dict.empty
            , links = []
            , entities =
                [ EmbeddedRepresentation entity1
                , EmbeddedRepresentation entity2
                , EmbeddedRepresentation entity3
                ]
            , actions = []
            }
    in
        describe "embeddedEntitiesWithClass"
            [ test "It returns the matching links" <|
                \() ->
                    Expect.equal [ entity1, entity2 ] (embeddedEntitiesWithClass "class1" entity)
            ]


embeddedEntitiesTests : Test
embeddedEntitiesTests =
    let
        entity1 =
            entityWithClasses [ "class1" ] "entity one"

        entity2 =
            entityWithClasses [ "class1", "class2" ] "entity two"

        entity3 =
            entityWithClasses [ "class2" ] "entity three"

        entity =
            { rels = Set.empty
            , classes = Set.empty
            , properties = Dict.empty
            , links = []
            , entities =
                [ EmbeddedRepresentation entity1
                , EmbeddedRepresentation entity2
                , EmbeddedRepresentation entity3
                ]
            , actions = []
            }
    in
        describe "embeddedEntitiesWithClass"
            [ test "It returns a a list of embedded entities" <|
                \() ->
                    Expect.equal [ entity1, entity2, entity3 ] (embeddedEntities entity)
            ]


linkWithClasses : List String -> String -> Link
linkWithClasses classes href =
    { rels = Set.singleton "rel"
    , classes = Set.fromList classes
    , href = href
    , title = Nothing
    , mediaType = Nothing
    }


linkWithRels : List String -> String -> Link
linkWithRels rels href =
    { rels = Set.fromList rels
    , classes = Set.empty
    , href = href
    , title = Nothing
    , mediaType = Nothing
    }


entityWithClasses : List String -> String -> Entity
entityWithClasses classes name =
    { rels = Set.empty
    , classes = Set.fromList classes
    , properties = Dict.singleton "name" <| StringValue name
    , links = []
    , entities = []
    , actions = []
    }
