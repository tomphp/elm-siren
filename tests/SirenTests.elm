module SirenTests exposing (..)

import Set exposing (Set)
import Test exposing (..)
import Expect
import Siren exposing (..)


all : Test
all =
    describe "Siren"
        [ describe "decodeJson"
            [ test "It decodes the rels" <|
                \() ->
                    Expect.equal
                        (Just (Entity (Set.fromList ["example1", "example2"])))
                        (decodeJson "{\"rel\": [\"example1\", \"example2\"]}")
            ]

        ]

