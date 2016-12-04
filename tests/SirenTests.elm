module SirenTests exposing (..)

import Dict exposing (Dict)
import Expect
import Maybe exposing (map)
import Set exposing (Set)
import Siren exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Siren"
        [ describe "decodeJson"
            [ describe "rels"
                [ test "It is a Set of rels" <|
                    \() ->
                        let
                            json =
                                "{\"rel\": [\"example1\", \"example2\"]}"
                        in
                            Expect.equal
                                (Just (Set.fromList [ "example1", "example2" ]))
                                (map (\e -> e.rels) (decodeJson json))
                , test "It is empty if the rel field was not defined" <|
                    \() ->
                        Expect.equal (Just Set.empty) (map (\e -> e.rels) (decodeJson "{}"))
                ]
            , describe "classes"
                [ test "It is a Set of classes" <|
                    \() ->
                        let
                            json =
                                "{\"class\": [\"example1\", \"example2\"]}"
                        in
                            Expect.equal
                                (Just (Set.fromList [ "example1", "example2" ]))
                                (map (\e -> e.classes) (decodeJson json))
                , test "It is empty if the class field was not defined" <|
                    \() ->
                        Expect.equal (Just Set.empty) (map (\e -> e.classes) (decodeJson "{}"))
                ]
            , describe "properties"
                [ test "It is an empty Dict if properties field is not defined" <|
                    \() ->
                        Expect.equal (Just Dict.empty) (map (\e -> e.properties) (decodeJson "{}"))
                , test "It can contain a string property" <|
                    \() ->
                        let
                            json =
                                "{\"properties\": {\"property-name\": \"string-value\"}}"
                        in
                            Expect.equal
                                (Just (Dict.singleton "property-name" (StringProperty "string-value")))
                                (map (\e -> e.properties) (decodeJson json))
                , test "It can contain an integer property" <|
                    \() ->
                        let
                            json =
                                "{\"properties\": {\"property-name\": 101}}"
                        in
                            Expect.equal
                                (Just (Dict.singleton "property-name" (IntProperty 101)))
                                (map (\e -> e.properties) (decodeJson json))
                ]
            ]
        ]
