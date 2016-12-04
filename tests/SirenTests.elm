module SirenTests exposing (..)

import Dict exposing (Dict)
import Expect
import Result exposing (map)
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
                                (Ok <| Set.fromList [ "example1", "example2" ])
                                (map rels <| decodeJson json)
                , test "It is empty if the rel field was not defined" <|
                    \() ->
                        Expect.equal (Ok Set.empty) (map rels <| decodeJson "{}")
                ]
            , describe "classes"
                [ test "It is a Set of classes" <|
                    \() ->
                        let
                            json =
                                "{\"class\": [\"example1\", \"example2\"]}"
                        in
                            Expect.equal
                                (Ok <| Set.fromList [ "example1", "example2" ])
                                (map classes <| decodeJson json)
                , test "It is empty if the class field was not defined" <|
                    \() ->
                        Expect.equal (Ok Set.empty) (map classes <| decodeJson "{}")
                ]
            , describe "properties"
                [ test "It is an empty Dict if properties field is not defined" <|
                    \() ->
                        Expect.equal (Ok Dict.empty) (map properties <| decodeJson "{}")
                , test "It can contain a string property" <|
                    \() ->
                        let
                            json =
                                "{\"properties\": {\"property-name\": \"string-value\"}}"
                        in
                            Expect.equal
                                (Ok <| Dict.singleton "property-name" <| StringValue "string-value")
                                (map properties <| decodeJson json)
                , test "It can contain an integer property" <|
                    \() ->
                        let
                            json =
                                "{\"properties\": {\"property-name\": 101}}"
                        in
                            Expect.equal
                                (Ok <| Dict.singleton "property-name" <| IntValue 101)
                                (map properties <| decodeJson json)
                , test "It can contain a float property" <|
                    \() ->
                        let
                            json =
                                "{\"properties\": {\"property-name\": 1.01}}"
                        in
                            Expect.equal
                                (Ok <| Dict.singleton "property-name" <| FloatValue 1.01)
                                (map properties <| decodeJson json)
                , test "It can contain a boolean property" <|
                    \() ->
                        let
                            json =
                                "{\"properties\": {\"property-name\": true}}"
                        in
                            Expect.equal
                                (Ok <| Dict.singleton "property-name" <| BoolValue True)
                                (map properties <| decodeJson json)
                , test "It can contain a null property" <|
                    \() ->
                        let
                            json =
                                "{\"properties\": {\"property-name\": null}}"
                        in
                            Expect.equal
                                (Ok <| Dict.singleton "property-name" NullValue)
                                (map properties <| decodeJson json)
                ]
            , describe "links"
                [ test "It is a dict of strings" <|
                    \() ->
                        let
                            json =
                                "{}"
                        in
                            Expect.equal
                                (Ok <| Dict.empty)
                                (map links <| decodeJson json)
                , test "Adds a link to the dictionary" <|
                    \() ->
                        let
                            json =
                                "{\"links\": [{\"rel\": [\"self\"], \"href\": \"http://example.com\"}]}"
                        in
                            Expect.equal
                                (Ok <| Dict.singleton "self" "http://example.com")
                                (map links <| decodeJson json)
                , test "Adds a link with multiple rels to the dictionary" <|
                    \() ->
                        let
                            json =
                                "{\"links\": [{\"rel\": [\"rel1\", \"rel2\"], \"href\": \"http://example.com\"}]}"
                        in
                            Expect.equal
                                (Ok <| Dict.fromList [("rel1", "http://example.com"), ("rel2", "http://example.com")])
                                (map links <| decodeJson json)
                ]
            ]
        ]
