module Tests exposing (..)

import Test exposing (..)
import Expect
import Helper


all : Test
all =
    describe "Helper Test Suite"
        [ describe "avgDiff"
            [ test "diff" <|
                \() -> Helper.diff [ 10, 6, 0 ] |> Expect.equal [ 4, 6 ]
            ]
        ]
