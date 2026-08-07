module VectorTest exposing (suite)

import Algebra.Vector as Vector
import Expect
import Field exposing (Field)
import Field.Finite as Finite exposing (Finite)
import FiniteFieldTest exposing (finite)
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    describe "Vector"
        (let
            field =
                Finite.field 37
         in
         [ test "vectors can be added" <|
            \_ ->
                let
                    left =
                        [ 1, 2, 3 ]
                            |> List.map field.fromInt
                            |> Vector.fromList

                    right =
                        [ 6, 5, 4 ]
                            |> List.map field.fromInt
                            |> Vector.fromList

                    actual =
                        Vector.add field left right

                    expected =
                        [ 7, 7, 7 ]
                            |> List.map field.fromInt
                            |> Vector.fromList
                in
                Expect.equal actual expected
         , test "vectors can be subtracted" <|
            \_ ->
                let
                    left =
                        [ 6, 5, 4 ]
                            |> List.map field.fromInt
                            |> Vector.fromList

                    right =
                        [ 1, 2, 3 ]
                            |> List.map field.fromInt
                            |> Vector.fromList

                    actual =
                        Vector.subtract field left right

                    expected =
                        [ 5, 3, 1 ]
                            |> List.map field.fromInt
                            |> Vector.fromList
                in
                Expect.equal actual expected
         , test "vectors can be scaled" <|
            \_ ->
                let
                    left =
                        [ 6, 5, 4 ]
                            |> List.map field.fromInt
                            |> Vector.fromList

                    scalar =
                        field.fromInt 3

                    actual =
                        Vector.scale field scalar left

                    expected =
                        [ 18, 15, 12 ]
                            |> List.map field.fromInt
                            |> Vector.fromList
                in
                Expect.equal actual expected
         ]
        )
