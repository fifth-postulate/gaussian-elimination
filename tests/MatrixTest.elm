module MatrixTest exposing (suite)

import Algebra.Matrix as Matrix exposing (Operation(..))
import Algebra.Vector as Vector
import Algebra.VectorSpace as VectorSpace
import Expect
import Field.Finite as Finite
import Test exposing (..)


suite : Test
suite =
    describe "Matrix"
        (let
            field =
                Finite.field 37
         in
         [ test "identity is already row echelon reduced" <|
            \_ ->
                let
                    start =
                        [ [ 1, 0, 0 ]
                        , [ 0, 1, 0 ]
                        , [ 0, 0, 1 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList

                    ( actual, _, _ ) =
                        Matrix.rowEchelon field start

                    expected =
                        [ [ 1, 0, 0 ]
                        , [ 0, 1, 0 ]
                        , [ 0, 0, 1 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList
                in
                Expect.equal actual expected
         , test "swapped rows can be row echelon reduced" <|
            \_ ->
                let
                    start =
                        [ [ 0, 1, 0 ]
                        , [ 1, 0, 0 ]
                        , [ 0, 0, 1 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList

                    ( actual, _, _ ) =
                        Matrix.rowEchelon field start

                    expected =
                        [ [ 1, 0, 0 ]
                        , [ 0, 1, 0 ]
                        , [ 0, 0, 1 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList
                in
                Expect.equal actual expected
         , test "swapped and scaled rows can be row echelon reduced" <|
            \_ ->
                let
                    start =
                        [ [ 0, 1, 0 ]
                        , [ 2, 0, 0 ]
                        , [ 0, 0, 1 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList

                    ( actual, _, _ ) =
                        Matrix.rowEchelon field start

                    expected =
                        [ [ 1, 0, 0 ]
                        , [ 0, 1, 0 ]
                        , [ 0, 0, 1 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList
                in
                Expect.equal actual expected
         , test "swapped, scaled and linear combination of rows can be row echelon reduced" <|
            \_ ->
                let
                    start =
                        [ [ 0, 1, 0 ]
                        , [ 2, 0, 0 ]
                        , [ 3, 0, 1 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList

                    ( actual, _, _ ) =
                        Matrix.rowEchelon field start

                    expected =
                        [ [ 1, 0, 0 ]
                        , [ 0, 1, 0 ]
                        , [ 0, 0, 1 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList
                in
                Expect.equal actual expected
         , test "operations are correctly calculated" <|
            \_ ->
                let
                    start =
                        [ [ 0, 1, 0 ]
                        , [ 2, 0, 0 ]
                        , [ 3, 0, 1 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList

                    ( _, actual, _ ) =
                        Matrix.rowEchelon field start

                    inverse2 =
                        2
                            |> field.fromInt
                            |> field.invertion
                            |> Maybe.withDefault field.one

                    expected =
                        [ Swap 0 1, Multiply inverse2 0, Linear (field.negation (field.fromInt 3)) 0 2 ]
                in
                Expect.equal actual expected
         , test "pivots are correctly calculated" <|
            \_ ->
                let
                    start =
                        [ [ 0, 0, 0, 1, 0, 0, 0 ]
                        , [ 2, 0, 0, 0, 0, 0, 0 ]
                        , [ 3, 0, 0, 0, 1, 0, 0 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList

                    ( _, _, actual ) =
                        Matrix.rowEchelon field start

                    expected =
                        [ 0, 3, 4 ]
                in
                Expect.equal actual expected
         , test "identity matrix can be created" <|
            \_ ->
                let
                    expected =
                        [ [ 1, 0, 0, 0 ]
                        , [ 0, 1, 0, 0 ]
                        , [ 0, 0, 1, 0 ]
                        , [ 0, 0, 0, 1 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList

                    actual =
                        Matrix.identity field 4
                in
                Expect.equal actual expected
         , test "transpose is calculated correctly" <|
            \_ ->
                let
                    expected =
                        [ [ 1, 2, 3, 4, 5 ]
                        , [ 6, 7, 8, 9, 10 ]
                        , [ 11, 12, 13, 14, 15 ]
                        , [ 16, 17, 18, 19, 20 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList

                    actual =
                        [ [ 1, 6, 11, 16 ]
                        , [ 2, 7, 12, 17 ]
                        , [ 3, 8, 13, 18 ]
                        , [ 4, 9, 14, 19 ]
                        , [ 5, 10, 15, 20 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList
                            |> Matrix.transpose field
                in
                Expect.equal actual expected
         , test "determine kernel of a matrix" <|
            \_ ->
                let
                    actual =
                        [ [ 1, 2, 3, 4, 5 ]
                        , [ 2, 4, 6, 8, 10 ]
                        , [ 0, 1, 2, 3, 4 ]
                        ]
                            |> List.map (List.map field.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList
                            |> Matrix.kernel field

                    expected =
                        [ 2, -1, 0 ]
                            |> List.map field.fromInt
                            |> Vector.fromList
                            |> List.singleton
                            |> VectorSpace.span field
                in
                Expect.equal True (VectorSpace.equals field actual expected)
         , test "ligths out application test" <|
            \_ ->
                let
                    f3 =
                        Finite.field 3

                    actual =
                        [ [ 1, 1, 1, 0, 0, 0 ]
                        , [ 1, 1, 0, 1, 0, 0 ]
                        , [ 1, 0, 1, 1, 1, 0 ]
                        , [ 0, 1, 1, 1, 0, 1 ]
                        , [ 0, 0, 1, 0, 1, 1 ]
                        , [ 0, 0, 0, 1, 1, 1 ]
                        ]
                            |> List.map (List.map f3.fromInt)
                            |> List.map Vector.fromList
                            |> Matrix.fromList
                            |> Matrix.kernel f3

                    expected =
                        [ 1, -1, 0, 0, -1, 1 ]
                            |> List.map f3.fromInt
                            |> Vector.fromList
                            |> List.singleton
                            |> VectorSpace.span f3
                in
                Expect.equal actual expected
         ]
        )
