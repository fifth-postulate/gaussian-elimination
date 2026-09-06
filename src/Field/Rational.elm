module Field.Rational exposing (Rational, field)

{-| The field of Rational numbers.

Allows you to calculate with rational numbers

    field = Field.Raational.field

    a =
        5
            |> field.fromInt
    b =
        8
            |> field.fromInt
            |> field.invertion
            |> Maybe.withDefault field.zero

    field.multiplication a b == 5/8

@docs Rational, field

-}

import Arithmetic exposing (egcd, sign)
import Field exposing (Field)


{-| Container element for a rational element
-}
type Rational
    = Element { numerator : Int, denominator : Int }


{-| Returns the field of rational numbers.
-}
field : Field Rational
field =
    { zero = fromInt 0
    , one = fromInt 1
    , fromInt = fromInt
    , addition = add
    , negation = negate
    , multiplication = multiply
    , invertion = invert
    }


fromInt : Int -> Rational
fromInt numerator =
    Element { numerator = numerator, denominator = 1 }


create_ : Int -> Int -> Rational
create_ numerator denominator =
    let
        n =
            abs numerator

        d =
            abs denominator

        s =
            sign numerator * sign denominator

        ( g, _, _ ) =
            egcd n d
    in
    Element { numerator = s * n // g, denominator = d // g }


add : Rational -> Rational -> Rational
add (Element left) (Element right) =
    create_ (right.denominator * left.numerator + left.denominator * right.numerator) (left.denominator * right.denominator)


negate : Rational -> Rational
negate (Element e) =
    Element { e | numerator = -1 * e.numerator }


multiply : Rational -> Rational -> Rational
multiply (Element left) (Element right) =
    create_ (left.numerator * right.numerator) (left.denominator * right.denominator)


invert : Rational -> Maybe Rational
invert (Element e) =
    if e.numerator == 0 then
        Nothing

    else
        { numerator = e.denominator, denominator = e.numerator }
            |> Element
            |> Just
