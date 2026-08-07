module Algebra.Vector exposing (Vector, add, dimension, fromList, scale, subtract)

import Field exposing (Field)
import General exposing (uncurry, zip)


type Vector a
    = Vector (List a)


fromList : List a -> Vector a
fromList =
    Vector


dimension : Vector a -> Int
dimension (Vector coordinates) =
    List.length coordinates


add : Field a -> Vector a -> Vector a -> Vector a
add field (Vector left) (Vector right) =
    zip left right
        |> List.map (uncurry field.addition)
        |> fromList


subtract : Field a -> Vector a -> Vector a -> Vector a
subtract field (Vector left) (Vector right) =
    right
        |> List.map field.negation
        |> zip left
        |> List.map (uncurry field.addition)
        |> fromList


scale : Field a -> a -> Vector a -> Vector a
scale field s (Vector coordinates) =
    coordinates
        |> List.map (field.multiplication s)
        |> fromList


dot : Field a -> Vector a -> Vector a -> a
dot field (Vector left) (Vector right) =
    zip left right
        |> List.map (uncurry field.multiplication)
        |> List.foldr field.addition field.zero


index : Int -> Vector a -> Maybe a
index n (Vector coordinates) =
    if n < 0 then
        Nothing

    else
        coordinates
            |> List.drop n
            |> List.head
