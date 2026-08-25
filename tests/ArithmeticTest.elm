module ArithmeticTest exposing (suite)

import Arithmetic exposing (inBase)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Arithmetic"
        [ test "inBase 2 3 10" <|
            \_ ->
                Expect.equal (inBase 2 3 10) [ 0, 1, 0 ]
        , test "inBase 2 4 10" <|
            \_ ->
                Expect.equal (inBase 2 4 10) [ 1, 0, 1, 0 ]
        , test "inBase 2 5 10" <|
            \_ ->
                Expect.equal (inBase 2 5 10) [ 0, 1, 0, 1, 0 ]
        , test "inBase 2 6 10" <|
            \_ ->
                Expect.equal (inBase 2 6 10) [ 0, 0, 1, 0, 1, 0 ]
        ]
