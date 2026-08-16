module Field exposing (Field)

{-| A [field](https://en.wikipedia.org/wiki/Field_(mathematics)) is

> a set on which addition, subtraction, multiplication, and division are defined

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
