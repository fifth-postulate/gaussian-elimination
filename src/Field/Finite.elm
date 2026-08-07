module Field.Finite exposing (Finite, field)

import Arithmetic exposing (egcd, sign)
import Field exposing (Field)


type Finite
    = Element { modulus : Int, class : Int }


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
