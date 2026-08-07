module Field.Rational exposing (Rational, field)

import Field exposing (Field)


type Rational
    = Element { numerator : Int, denominator : Int }


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


create : Int -> Int -> Maybe Rational
create numerator denominator =
    if denominator == 0 then
        Nothing

    else
        Just (create_ numerator denominator)


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


egcd : Int -> Int -> ( Int, Int, Int )
egcd a b =
    let
        -- invariant: x = u * a + v * b && y = s * a + t * b
        go : Int -> Int -> Int -> Int -> Int -> Int -> ( Int, Int, Int )
        go x y u v s t =
            if y == 0 then
                ( x, u, v )

            else
                let
                    q =
                        x // y
                in
                go y (x - q * y) s t (u - q * s) (v - q * t)
    in
    go (abs a) (abs b) (sign a) 0 0 (sign b)


sign : Int -> Int
sign n =
    case compare n 0 of
        LT ->
            -1

        EQ ->
            0

        GT ->
            1
