module VectorSpaceAllTest exposing (suite)

import Algebra.Vector as Vector
import Algebra.VectorSpace as VectorSpace
import Expect exposing (Expectation)
import Field.Finite as Finite
import Fuzz exposing (..)
import Test exposing (..)
import VectorSpaceTest exposing (vectorSpace)


suite : Test
suite =
    describe "VectorSpace"
        (let
            field =
                Finite.field 3
         in
         [ describe "all"
            [ test "orthonormal base" <|
                \_ ->
                    let
                        actual =
                            [ [ 1, 0, 0 ]
                            , [ 0, 1, 0 ]
                            ]
                                |> vectorSpace field
                                |> VectorSpace.all (Vector.toList >> List.map Finite.toInt) field

                        expected =
                            [ [ 0, 0, 0 ]
                            , [ 0, 1, 0 ]
                            , [ 0, 2, 0 ]
                            , [ 1, 0, 0 ]
                            , [ 1, 1, 0 ]
                            , [ 1, 2, 0 ]
                            , [ 2, 0, 0 ]
                            , [ 2, 1, 0 ]
                            , [ 2, 2, 0 ]
                            ]
                    in
                    sameElements actual expected
            , test "a base" <|
                \_ ->
                    let
                        actual =
                            [ [ 1, 1, 0 ]
                            , [ 0, 1, 0 ]
                            ]
                                |> vectorSpace field
                                |> VectorSpace.all (Vector.toList >> List.map Finite.toInt) field

                        expected =
                            [ [ 0, 0, 0 ]
                            , [ 0, 1, 0 ]
                            , [ 0, 2, 0 ]
                            , [ 1, 0, 0 ]
                            , [ 1, 1, 0 ]
                            , [ 1, 2, 0 ]
                            , [ 2, 0, 0 ]
                            , [ 2, 1, 0 ]
                            , [ 2, 2, 0 ]
                            ]
                    in
                    sameElements actual expected
            ]
         ]
        )


sameElements : List a -> List a -> Expectation
sameElements left right =
    let
        allLeftPresentInRight =
            List.all (\l -> List.member l right) left

        allRightPresentInLeft =
            List.all (\r -> List.member r left) right

        sameSize =
            List.length left == List.length right
    in
    if allLeftPresentInRight && allRightPresentInLeft && sameSize then
        Expect.pass

    else
        Expect.fail "not the same elements"
