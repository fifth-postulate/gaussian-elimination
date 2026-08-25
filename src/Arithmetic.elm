module Arithmetic exposing (egcd, inBase, sign)


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


inBase : Int -> Int -> Int -> List Int
inBase base digits n =
    let
        go : List Int -> Int -> Int -> List Int
        go acc d remainder =
            if d >= digits then
                acc

            else
                let
                    digit =
                        modBy base remainder
                in
                go (digit :: acc) (d + 1) (remainder // base)
    in
    go [] 0 n
