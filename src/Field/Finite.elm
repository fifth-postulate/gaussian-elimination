module Field.Finite exposing
    ( Finite, field
    , order, toInt
    )

{-| The field of Finite numbers.

These finite fields aren't as general as can be. These finie fields only
can have a prime number of elements.

Below we will show that in the finite field with 5 elements 2 and 3 are
mutlplicative inverses.

    field = Field.Finite.field 5

    two = field.fromInt 2
    three = field.fromInt 3

    field.multiplication two three == field.fromInt 1

@docs Finite, field


## Inspection

@docs order, toInt

-}

import Arithmetic exposing (egcd)
import Field exposing (Field)


{-| Container for a finite number.
-}
type Finite
    = Element { modulus : Int, class : Int }


{-| Returns a finite field.

Currently only a prime number of elements is supported. The client must ensure
the number of elements is prime.

-}
field : Int -> Field Finite
field modulus =
    { zero = fromInt modulus 0
    , one = fromInt modulus 1
    , fromInt = fromInt modulus
    , addition = add
    , negation = negate
    , multiplication = multiply
    , invertion = invert
    }


fromInt : Int -> Int -> Finite
fromInt modulus class =
    Element { modulus = modulus, class = modBy modulus class }


add : Finite -> Finite -> Finite
add (Element left) (Element right) =
    fromInt left.modulus (left.class + right.class)


negate : Finite -> Finite
negate (Element e) =
    fromInt e.modulus (e.modulus - e.class)


multiply : Finite -> Finite -> Finite
multiply (Element left) (Element right) =
    fromInt left.modulus (left.class * right.class)


invert : Finite -> Maybe Finite
invert (Element e) =
    if e.class == 0 then
        Nothing

    else
        let
            ( _, _, inverse ) =
                egcd e.modulus e.class
        in
        inverse
            |> fromInt e.modulus
            |> Just


{-| Determine the number of elements in the finite field.
-}
order : Field Finite -> Int
order finiteField =
    case finiteField.zero of
        Element { modulus } ->
            modulus


{-| Dtermine in which class the element belangs to.
-}
toInt : Finite -> Int
toInt element =
    case element of
        Element { class } ->
            class
