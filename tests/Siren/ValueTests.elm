module Siren.ValueTests exposing (..)

import Dict
import Expect
import Set
import Siren.Value exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Siren.Value"
        [ toStringTests
        ]


toStringTests : Test
toStringTests =
    describe "toString"
        [ test "It converts a StringValue" <|
            \() ->
                Expect.equal "example" (StringValue "example" |> Siren.Value.toString)
        , test "It converts a IntValue" <|
            \() ->
                Expect.equal "123" (IntValue 123 |> Siren.Value.toString)
        , test "It converts a FloatValue" <|
            \() ->
                Expect.equal "9.99" (FloatValue 9.99 |> Siren.Value.toString)
        , test "It converts a True BoolValue" <|
            \() ->
                Expect.equal "True" (BoolValue True |> Siren.Value.toString)
        , test "It converts a False BoolValue" <|
            \() ->
                Expect.equal "False" (BoolValue False |> Siren.Value.toString)
        , test "It converts a NullValue" <|
            \() ->
                Expect.equal "Null" (NullValue |> Siren.Value.toString)
        ]
