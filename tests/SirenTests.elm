module SirenTests exposing (..)

import Set exposing (Set)
import Test exposing (..)
import Expect
import Siren exposing (..)


all : Test
all =
    describe "Siren"
        [ describe "decodeJson"
            [ describe "rels"
                [ test "It is a Set of rels" <|
                    \() ->
                        Expect.equal
                            (Just (Entity (Set.fromList [ "example1", "example2" ]) Set.empty))
                            (decodeJson "{\"rel\": [\"example1\", \"example2\"]}")
                , test "It is empty if the rel field was not defined" <|
                    \() ->
                        Expect.equal
                            (Just (Entity Set.empty Set.empty))
                            (decodeJson "{}")
                ]
            , describe "classes"
                [ test "It is a Set of classes" <|
                    \() ->
                        Expect.equal
                            (Just (Entity Set.empty (Set.fromList [ "example1", "example2" ])))
                            (decodeJson "{\"class\": [\"example1\", \"example2\"]}")
                , test "It is empty if the class field was not defined" <|
                    \() ->
                        Expect.equal
                            (Just (Entity Set.empty Set.empty))
                            (decodeJson "{}")
                ]
            ]
        ]
