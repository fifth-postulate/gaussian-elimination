module MatrixTest exposing (suite)

import Algebra.Matrix as Matrix exposing (Operation(..))
import Algebra.Vector as Vector
import Expect
import Field exposing (Field)
import Field.Finite as Finite exposing (Finite)
import FiniteFieldTest exposing (finite)
import Fuzz exposing (Fuzzer)
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
         ]
        )
