module SirenTests exposing (..)

import Dict
import Expect
import Set
import Siren exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Siren"
        [ linksByClassTests
        ]


linksByClassTests : Test
linksByClassTests =
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
        describe "linksByClass"
            [ test "It returns an empty list when no match is found" <|
                \() ->
                    Expect.equal [] (linksByClass "unknown" entity)
            , test "It returns the matching links" <|
                \() ->
                    Expect.equal [ link1, link2 ] (linksByClass "class1" entity)
            ]


linkWithClasses : List String -> String -> Link
linkWithClasses classes href =
    { rels = Set.singleton "rel"
    , classes = Set.fromList classes
    , href = href
    , title = Nothing
    , mediaType = Nothing
    }
