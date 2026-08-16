module General exposing
    ( zip, swivel
    , uncurry, swap
    )

{-| This module provide a number of utility functions


## Working with Lists

@docs zip, swivel


## Working with functions

@docs uncurry, swap

-}


{-| Zip two lists together.

The resulting list is as long as the shortes list.

-}
zip : List a -> List b -> List ( a, b )
zip xs ys =
    let
        go : List ( a, b ) -> List a -> List b -> List ( a, b )
        go acc us vs =
            case ( us, vs ) of
                ( u :: uss, v :: vss ) ->
                    go (( u, v ) :: acc) uss vss

                _ ->
                    List.reverse acc
    in
    go [] xs ys


{-| Transform a curried function into a function with a tuple as argument.
-}
uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


{-| Rotate n elements of a list from the head to the tail.
-}
swivel : Int -> List a -> List a
swivel n us =
    let
        n_ =
            if n < 0 then
                List.length us + n

            else
                n

        hs =
            List.take n_ us

        ts =
            List.drop n_ us
    in
    List.append ts hs


{-| Swap the order of first two arguments of a function.
-}
swap : (b -> a -> c) -> a -> b -> c
swap f a b =
    f b a
