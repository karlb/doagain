module Tests exposing (..)

import Expect
import Helper
import Test exposing (..)


all : Test
all =
    describe "Helper Test Suite"
        [ describe "avgDiff"
            [ test "diff" <|
                \() -> Helper.diff [ 10, 6, 0 ] |> Expect.equal [ 4, 6 ]
            ]
        ]
