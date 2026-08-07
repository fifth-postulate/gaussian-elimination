module FiniteFieldTest exposing (suite)

import Expect
import Field exposing (Field)
import Field.Finite as Finite exposing (Finite)
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    describe "Finite Field"
        (let
            modulus =
                37

            field =
                Finite.field modulus
         in
         [ describe "addition"
            [ fuzz (finite modulus field) "zero is an identity" <|
                \start ->
                    Expect.all
                        [ \left -> Expect.equal (field.addition left start) start
                        , \right -> Expect.equal (field.addition start right) start
                        ]
                        field.zero
            , fuzz (finite modulus field) "each element has a negative" <|
                \start ->
                    let
                        negative =
                            field.negation start
                    in
                    Expect.all
                        [ \left -> Expect.equal (field.addition left start) field.zero
                        , \right -> Expect.equal (field.addition start right) field.zero
                        ]
                        negative
            ]
         , describe "multiplication"
            [ fuzz (finite modulus field) "one is an identity" <|
                \start ->
                    Expect.all
                        [ \left -> Expect.equal (field.multiplication left start) start
                        , \right -> Expect.equal (field.multiplication start right) start
                        ]
                        field.one
            , fuzz (finite modulus field) "each element, besides zero, has an inverse" <|
                \start ->
                    case field.invertion start of
                        Nothing ->
                            Expect.equal start field.zero

                        Just inverse ->
                            Expect.all
                                [ \left -> Expect.equal (field.multiplication left start) field.one
                                , \right -> Expect.equal (field.multiplication start right) field.one
                                ]
                                inverse
            ]
         ]
        )


finite : Int -> Field Finite -> Fuzzer Finite
finite modulus field =
    modulus
        |> Fuzz.uniformInt
        |> Fuzz.map field.fromInt
