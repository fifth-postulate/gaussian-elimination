module VectorSpaceTest exposing (suite, vectorSpace)

import Algebra.Vector as Vector
import Algebra.VectorSpace as VectorSpace exposing (VectorSpace)
import Expect exposing (Expectation)
import Field exposing (Field)
import Field.Finite as Finite exposing (Finite)
import Fuzz exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "VectorSpace"
        (let
            field =
                Finite.field 37
         in
         [ describe "creation"
            [ test "span creates a VectorSpace" <|
                \_ ->
                    let
                        v =
                            [ 1, 2, 3 ]
                                |> List.map field.fromInt
                                |> Vector.fromList

                        actual =
                            vectorSpace field [ [ 1, 2, 3 ] ]

                        expected =
                            VectorSpace.empty
                                |> VectorSpace.add field v
                    in
                    Expect.equal actual expected
            ]
         , describe "contains"
            [ test "vector which is a multiple of a single vector is contained" <|
                \_ ->
                    let
                        b =
                            [ 1, 2, 3 ]
                                |> List.map field.fromInt
                                |> Vector.fromList

                        v =
                            Vector.scale field (field.fromInt 2) b

                        space =
                            VectorSpace.span field [ b ]
                    in
                    Expect.equal True (VectorSpace.contains field v space)
            , test "vector which is a sum of two vectors is contained" <|
                \_ ->
                    let
                        b1 =
                            [ 1, 2, 3 ]
                                |> List.map field.fromInt
                                |> Vector.fromList

                        b2 =
                            [ 3, 1, 3 ]
                                |> List.map field.fromInt
                                |> Vector.fromList

                        v =
                            Vector.add field b1 b2

                        space =
                            VectorSpace.span field [ b1, b2 ]
                    in
                    Expect.equal True (VectorSpace.contains field v space)
            ]
         , describe "intersection"
            [ test "works as expected" <|
                \_ ->
                    let
                        u =
                            vectorSpace field [ [ 1, 0, 0 ], [ 0, 1, 0 ] ]

                        v =
                            vectorSpace field [ [ 1, 1, -1 ], [ 1, 1, 1 ] ]

                        actual =
                            VectorSpace.intersection field u v

                        expected =
                            vectorSpace field [ [ 1, 1, 0 ] ]
                    in
                    Expect.equal True (VectorSpace.equals field actual expected)
            ]
         ]
        )


vectorSpace : Field a -> List (List Int) -> VectorSpace a
vectorSpace field vectors =
    vectors
        |> List.map (List.map field.fromInt >> Vector.fromList)
        |> VectorSpace.span field
