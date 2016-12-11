module SirenTests exposing (..)

import Dict
import Expect
import Set
import Siren exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Siren"
        [ linksWithRelTests
        , linksWithClassTests
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
            Entity
                Set.empty
                Set.empty
                Dict.empty
                [ link1, link2, link3 ]
                []
                []
    in
        describe "linksWithClass"
            [ test "It returns an empty list when no match is found" <|
                \() ->
                    Expect.equal [] (linksWithRel "unknown" entity)
            , test "It returns the matching links" <|
                \() ->
                    Expect.equal [ link1, link2 ] (linksWithRel "rel1" entity)
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
            Entity
                Set.empty
                Set.empty
                Dict.empty
                [ link1, link2, link3 ]
                []
                []
    in
        describe "linksWithClass"
            [ test "It returns an empty list when no match is found" <|
                \() ->
                    Expect.equal [] (linksWithClass "unknown" entity)
            , test "It returns the matching links" <|
                \() ->
                    Expect.equal [ link1, link2 ] (linksWithClass "class1" entity)
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
