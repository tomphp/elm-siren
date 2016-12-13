port module Main exposing (..)

import Test exposing (describe)
import SirenTests
import Siren.DecodeTests
import Siren.EntityTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit <|
        describe "Elm Siren"
            [ SirenTests.all
            , Siren.EntityTests.all
            , Siren.DecodeTests.all
            ]


port emit : ( String, Value ) -> Cmd msg
