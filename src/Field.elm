module Field exposing (Field)

{-| A [field](https://en.wikipedia.org/wiki/Field_%28mathematics%29) is

> a set on which addition, subtraction, multiplication, and division are defined

Basically it allows you to do the usual operations you expect of "numbers".


## Example

Below we will show that in the finite field with 5 elements 2 and 3 are
mutlplicative inverses.

    field = Field.Finite 5

    two = field.fromInt 2
    three = field.fromInt 3

    field.multiplication two three == field.fromInt 1

See Field.Finite and Field.Rational for specific examples.

@docs Field

-}


{-| A record with all field operations.

It is parameterized of the type of elements the field has operations on.

-}
type alias Field a =
    { zero : a
    , one : a
    , fromInt : Int -> a
    , addition : a -> a -> a
    , negation : a -> a
    , multiplication : a -> a -> a
    , invertion : a -> Maybe a
    }
