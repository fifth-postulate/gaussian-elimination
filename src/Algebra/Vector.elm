module Algebra.Vector exposing
    ( Vector
    , fromList, zero
    , dimension, index
    , add, dot, scale, subtract
    , toList
    )

{-| This module provides a Vector type.

Vectors are parameterized over their container element and you should provide
a Field with the operations.

The examples assume a field

    field =
        Field.Finite.field 5


## Type

@docs Vector


## Construction

@docs fromList, zero


## Inspection

@docs dimension, index


## Operation

@docs add, dot, scale, subtract


## Deconstruction

@docs toList

-}

import Field exposing (Field)
import General exposing (uncurry, zip)


{-| The Vector type

This type is parameterized over its elements.

-}
type Vector a
    = Vector (List a)


{-| Create a Vector from a list of elements

    field = Finite.Field.field 5

    v =
        [1, 2, 3, 4, 5, 6]
            |> List.map field.fromInt
            |> Vector.fromList

    Vector.toList v == [1, 2, 3, 4, 0, 1]

-}
fromList : List a -> Vector a
fromList =
    Vector


{-| Returns the number of coordinates in the given Vector.

    v =
        [1,2,3]
            |> List.map field.fromInt
            |> Vector.fromList

    Vector.dimension v == 3

-}
dimension : Vector a -> Int
dimension (Vector coordinates) =
    List.length coordinates


{-| Create the zero Vector in a certain dimension.

It uses the zero element of the field.

    v = Vector.zero field 4

    Vector.toList v == [0, 0, 0, 0]

-}
zero : Field a -> Int -> Vector a
zero field d =
    List.repeat d field.zero
        |> fromList


{-| Add two vectors component-wise.

    field = Field.Finite.field 5
    a =
        [1, 2, 3]
            |> List.map field.fromInt
            |> Vector.fromList
    b =
        [3, 4, 5]
            |> List.map field.fromInt
            |> Vector.fromList

    Vector.add field a b == [4, 1, 3]

-}
add : Field a -> Vector a -> Vector a -> Vector a
add field (Vector left) (Vector right) =
    zip left right
        |> List.map (uncurry field.addition)
        |> fromList


{-| Subtract two vectors.

This is a convenience method. It could be implemented with field negation
and the add function.

-}
subtract : Field a -> Vector a -> Vector a -> Vector a
subtract field (Vector left) (Vector right) =
    right
        |> List.map field.negation
        |> zip left
        |> List.map (uncurry field.addition)
        |> fromList


{-| Scalar multiplication

    field = Field.Finite.field 5
    a =
        [1, 2, 3]
            |> List.map field.fromInt
            |> Vector.fromList
    s = field.fromInt 3

    Vector.add field s a == [3, 1, 4]

-}
scale : Field a -> a -> Vector a -> Vector a
scale field s (Vector coordinates) =
    coordinates
        |> List.map (field.multiplication s)
        |> fromList


{-| The inner-product of two vectors.

The dimension of the result is equal to the smallest dimension of the
arguments.

        field = Field.Finite.field 5
        a =
            [1, 2, 3]
                |> List.map field.fromInt
                |> Vector.fromList

        Vector.dot field a a == 4

-}
dot : Field a -> Vector a -> Vector a -> a
dot field (Vector left) (Vector right) =
    zip left right
        |> List.map (uncurry field.multiplication)
        |> List.foldr field.addition field.zero


{-| Return the i-th component of a Vector, if it exists.

Nohting otherwise.

        field = Field.Finite.field 5
        a =
            [1, 2, 3]
                |> List.map field.fromInt
                |> Vector.fromList

        Vector.index 1 a == Just 2

-}
index : Int -> Vector a -> Maybe a
index n (Vector coordinates) =
    if n < 0 then
        Nothing

    else
        coordinates
            |> List.drop n
            |> List.head


{-| Return the coordinates of the vector.
-}
toList : Vector a -> List a
toList (Vector coordinates) =
    coordinates
