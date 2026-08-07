module Field.Finite exposing (Finite, field)

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
