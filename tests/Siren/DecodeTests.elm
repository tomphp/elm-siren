module Siren.DecodeTests exposing (..)

import Dict
import Expect
import Json.Decode exposing (decodeString)
import Result
import Set
import Siren exposing (..)
import Siren.Decode exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Siren.Decode"
        [ valueTests
        , linkTests
        , actionFieldTests
        , actionTests
        , entityLinkTests
        , entityTests
        , decodeJsonTests
        ]


valueTests : Test
valueTests =
    describe "value"
        [ test "It can decode a string" <|
            \() ->
                Expect.equal
                    (Ok <| StringValue "string-value")
                    (decodeString value "\"string-value\"")
        , test "It can decode an integer" <|
            \() ->
                Expect.equal
                    (Ok <| IntValue 101)
                    (decodeString value "101")
        , test "It can decode a float" <|
            \() ->
                Expect.equal
                    (Ok <| FloatValue 1.01)
                    (decodeString value "1.01")
        , test "It can decode a boolean" <|
            \() ->
                Expect.equal
                    (Ok <| BoolValue True)
                    (decodeString value "true")
        , test "It can decode a null" <|
            \() ->
                Expect.equal (Ok NullValue) (decodeString value "null")
        ]


linkTests : Test
linkTests =
    describe "link"
        [ test "It decodes a link" <|
            \() ->
                let
                    json =
                        """
                        {
                            "rel": ["rel1", "rel2"],
                            "href": "http://example.com/api/entities/123"
                        }
                        """
                in
                    Expect.equal
                        (Ok <|
                            Link
                                (Set.fromList [ "rel1", "rel2" ])
                                "http://example.com/api/entities/123"
                        )
                        (decodeString link json)
        ]


actionFieldTests : Test
actionFieldTests =
    describe "actionField"
        [ test "It decodes a field with minimum details" <|
            \() ->
                let
                    json =
                        "{\"name\": \"the-field\"}"
                in
                    Expect.equal
                        (Ok <|
                            Field
                                "the-field"
                                Set.empty
                                "text"
                                Nothing
                                Nothing
                        )
                        (decodeString actionField json)
        , test "It decodes a field with all details" <|
            \() ->
                let
                    json =
                        """
                        {
                            "name": "the-field",
                            "class": ["the-class"],
                            "type": "email",
                            "value": "something@example.com",
                            "title": "The Field"
                        }
                        """
                in
                    Expect.equal
                        (Ok <|
                            Field
                                "the-field"
                                (Set.singleton "the-class")
                                "email"
                                (Just "something@example.com")
                                (Just "The Field")
                        )
                        (decodeString actionField json)
        ]


actionTests : Test
actionTests =
    describe "action"
        [ test "It decodes an action with minimum details" <|
            \() ->
                let
                    json =
                        """
                        {
                            "name": "action-name",
                            "href": "http://example.com/api/entities/456"
                        }
                        """
                in
                    Expect.equal
                        (Ok <|
                            Action
                                "action-name"
                                "http://example.com/api/entities/456"
                                Set.empty
                                "GET"
                                Nothing
                                []
                        )
                        (decodeString action json)
        , test "It decodes an action with minimum details" <|
            \() ->
                let
                    json =
                        """
                        {
                            "name": "action-name",
                            "href": "http://example.com/api/entities/456"
                        }
                        """
                in
                    Expect.equal
                        (Ok <|
                            Action
                                "action-name"
                                "http://example.com/api/entities/456"
                                Set.empty
                                "GET"
                                Nothing
                                []
                        )
                        (decodeString action json)
        , test "It decodes an action with full details" <|
            \() ->
                let
                    json =
                        """
                        {
                            "name": "action-name",
                            "href": "http://example.com",
                            "class": ["action-class"],
                            "method": "POST",
                            "title": "The Action"
                        }
                        """
                in
                    Expect.equal
                        (Ok <|
                            Action
                                "action-name"
                                "http://example.com"
                                (Set.singleton "action-class")
                                "POST"
                                (Just "The Action")
                                []
                        )
                        (decodeString action json)
        , test "It decodes an action with fields" <|
            \() ->
                let
                    json =
                        """
                        {
                            "name": "action-name",
                            "href": "http://example.com/api/entities/456",
                            "fields": [
                                {"name": "field-name"}
                            ]
                        }
                        """
                in
                    Expect.equal
                        (Ok <|
                            [ Field
                                "field-name"
                                Set.empty
                                "text"
                                Nothing
                                Nothing
                            ]
                        )
                        (decodeString action json |> Result.map .fields)
        ]


entityLinkTests : Test
entityLinkTests =
    describe "entityLink"
        [ test "It decodes an entityLink with minimum details" <|
            \() ->
                Expect.equal
                    (Ok <|
                        EntityLink
                            Set.empty
                            Set.empty
                            "http://example.com/api/entities/55"
                            Nothing
                            Nothing
                    )
                    (decodeString entityLink "{\"href\": \"http://example.com/api/entities/55\"}")
        , test "It decodes an entityLink all details" <|
            \() ->
                let
                    json =
                        """
                        {
                            "rel": ["the-rel"],
                            "class": ["the-class"],
                            "href": "http://example.com/api/entities/77",
                            "type": "the-type",
                            "title": "The Title"
                        }
                        """
                in
                    Expect.equal
                        (Ok <|
                            EntityLink
                                (Set.singleton "the-rel")
                                (Set.singleton "the-class")
                                "http://example.com/api/entities/77"
                                (Just "the-type")
                                (Just "The Title")
                        )
                        (decodeString entityLink json)
        ]


entityTests : Test
entityTests =
    describe "entity"
        [ test "It decodes an entity with minimum details" <|
            \() ->
                Expect.equal
                    (Ok <|
                        Entity
                            Set.empty
                            Set.empty
                            Dict.empty
                            []
                            []
                            []
                    )
                    (decodeString entity "{}")
        , test "It decodes an entity all details" <|
            \() ->
                let
                    json =
                        """
                        {
                            "rel": ["the-rel"],
                            "class": ["the-class"],
                            "properties": {"the-property": "the-value"},
                            "links": [{
                                "rel": ["link-rel"],
                                "href": "http://example.com/api/entities"
                            }],
                            "actions": [{
                                "name": "action-name",
                                "href": "http://example.com/api/entities/123"
                            }]
                        }
                        """
                in
                    Expect.equal
                        (Ok <|
                            Entity
                                (Set.singleton "the-rel")
                                (Set.singleton "the-class")
                                (Dict.singleton "the-property" (StringValue "the-value"))
                                [ Link
                                    (Set.singleton "link-rel")
                                    "http://example.com/api/entities"
                                ]
                                []
                                [ Action
                                    "action-name"
                                    "http://example.com/api/entities/123"
                                    Set.empty
                                    "GET"
                                    Nothing
                                    []
                                ]
                        )
                        (decodeString entity json)
        , test "It decodes an entity with an embedded entity" <|
            \() ->
                let
                    json =
                        """
                        {
                            "entities": [
                                {"class": ["embedded"]}
                            ]
                        }
                        """
                in
                    Expect.equal
                        (Ok <|
                            Entity
                                Set.empty
                                Set.empty
                                Dict.empty
                                []
                                [ Entity
                                    Set.empty
                                    (Set.singleton "embedded")
                                    Dict.empty
                                    []
                                    []
                                    []
                                ]
                                []
                        )
                        (decodeString entity json)
        , test "It decodes an entity with an embedded entity link" <|
            \() ->
                let
                    json =
                        """
                        {
                            "entities": [
                                {"href": "http://example.com/embedded"}
                            ]
                        }
                        """
                in
                    Expect.equal
                        (Ok <|
                            Entity
                                Set.empty
                                Set.empty
                                Dict.empty
                                []
                                [ EntityLink
                                    Set.empty
                                    Set.empty
                                    "http://example.com/embedded"
                                    Nothing
                                    Nothing
                                ]
                                []
                        )
                        (decodeString entity json)
        ]


decodeJsonTests : Test
decodeJsonTests =
    describe "decodeJson"
        [ test "It decodes an entity" <|
            \() ->
                let
                    json =
                        """
                        {
                            "rel": ["the-rel"],
                            "class": ["the-class"],
                            "properties": {"the-property": "the-value"},
                            "links": [{
                                "rel": ["link-rel"],
                                "href": "http://example.com/api/entities"
                            }],
                            "actions": [{
                                "name": "action-name",
                                "href": "http://example.com/api/entities/123"
                            }]
                        }
                        """
                in
                    Expect.equal
                        (decodeString entity json)
                        (decodeJson json)
        ]
