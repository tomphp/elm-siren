port module Main exposing (..)

import Test exposing (describe)
import SirenTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit <|
        describe "Elm Siren"
            [ SirenTests.all ]


port emit : ( String, Value ) -> Cmd msg
