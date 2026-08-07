module FieldOfRationalsTest exposing (suite)

import Expect
import Field exposing (Field)
import Field.Rational as Rational exposing (Rational)
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    describe "Rationals"
        (let
            field =
                Rational.field
         in
         [ describe "addition"
            [ fuzz (rational field) "zero is an identity" <|
                \start ->
                    Expect.all
                        [ \left -> Expect.equal (field.addition left start) start
                        , \right -> Expect.equal (field.addition start right) start
                        ]
                        field.zero
            , fuzz (rational field) "each element has a negative" <|
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
            [ fuzz (rational field) "one is an identity" <|
                \start ->
                    Expect.all
                        [ \left -> Expect.equal (field.multiplication left start) start
                        , \right -> Expect.equal (field.multiplication start right) start
                        ]
                        field.one
            , fuzz (rational field) "each element, besides zero, has an inverse" <|
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


rational : Field Rational -> Fuzzer Rational
rational field =
    let
        numerator =
            Fuzz.intRange -100 100
                |> Fuzz.map field.fromInt

        denominator =
            Fuzz.intRange 1 100
                |> Fuzz.map field.fromInt
                |> Fuzz.map field.invertion
                |> Fuzz.filter isJust
                |> Fuzz.map (Maybe.withDefault field.zero)
    in
    Fuzz.map2 field.multiplication numerator denominator


isJust : Maybe a -> Bool
isJust value =
    value
        |> Maybe.map (always True)
        |> Maybe.withDefault False
