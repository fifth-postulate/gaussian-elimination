module Algebra.VectorSpace exposing
    ( VectorSpace
    , span, empty
    , equals
    , add, contains, intersection, all
    )

{-| Provide a VectorSpace.

A VectorSpace is

> a set whose elements, often called vectors, can be added together and multiplied ("scaled") by numbers called scalars


## Type

@docs VectorSpace


## Creation

@docs span, empty


## Equality

@docs equals


## Operation

@docs add, contains, intersection, all

-}

import Algebra.Vector as Vector exposing (Vector)
import Arithmetic exposing (inBase)
import Field exposing (Field)
import Field.Finite as Finite exposing (Finite)
import General exposing (swap, uncurry, zip)


{-| The VectorSpace type.

It is parameterized over the elements its vectors contains.

-}
type VectorSpace a
    = Span { basis : List (Vector a) }
    | Origin


{-| Create the VectorSpace that is spanned by the list of vectors.
-}
span : Field a -> List (Vector a) -> VectorSpace a
span field basis =
    List.foldl (add field) empty basis


{-| The empty VectorSpace.
-}
empty : VectorSpace a
empty =
    Origin


{-| Determinis if a Vector is contiained in the VectorSpace
-}
contains : Field a -> Vector a -> VectorSpace a -> Bool
contains field v space =
    let
        w =
            projection field v space
    in
    v == w


projection : Field a -> Vector a -> VectorSpace a -> Vector a
projection field v space =
    case space of
        Span { basis } ->
            let
                coefficients =
                    basis
                        |> List.map (project field v)

                components =
                    zip coefficients basis
                        |> List.map (uncurry (Vector.scale field))
            in
            case components of
                c :: cs ->
                    List.foldl (Vector.add field) c cs

                [] ->
                    v

        Origin ->
            Vector.zero field (Vector.dimension v)


project : Field a -> Vector a -> Vector a -> a
project field v b =
    Vector.dot field b b
        |> field.invertion
        |> Maybe.map (field.multiplication (Vector.dot field b v))
        |> Maybe.withDefault field.zero


{-| Add a Vector to the basis of the VectorSpace.
-}
add : Field a -> Vector a -> VectorSpace a -> VectorSpace a
add field v space =
    if contains field v space then
        space

    else
        case space of
            Span vs ->
                let
                    p =
                        projection field v space

                    n =
                        Vector.subtract field v p
                in
                { vs | basis = List.append vs.basis [ n ] }
                    |> Span

            Origin ->
                Span { basis = [ v ] }


{-| The intersection of two VectorSpaces.

This is formed by the linear space of all the vectors that are contained in both
VectorSpaces.

-}
intersection : Field a -> VectorSpace a -> VectorSpace a -> VectorSpace a
intersection field u v =
    case ( u, v ) of
        ( Span left, Span _ ) ->
            left.basis
                |> List.map (swap (projection field) v)
                |> span field

        _ ->
            Origin


{-| Determines if two VectorSpaces are the same linear spaces.
-}
equals : Field a -> VectorSpace a -> VectorSpace a -> Bool
equals field u v =
    isSubspace field u v && isSubspace field v u


isSubspace : Field a -> VectorSpace a -> VectorSpace a -> Bool
isSubspace field u v =
    case u of
        Span { basis } ->
            List.all (swap (contains field) v) basis

        Origin ->
            True


{-| Iterate over all Vectors in this VectorSpace.

Apply a transformation on each Vector and return the result as a list.

-}
all : (Vector Finite -> b) -> Field Finite -> VectorSpace Finite -> List b
all f field space =
    case space of
        Span { basis } ->
            let
                d =
                    List.length basis

                modulus =
                    Finite.order field
            in
            List.range 0 ((modulus ^ d) - 1)
                |> List.map (inBase modulus d)
                |> List.map (List.map field.fromInt)
                |> List.map (toVector field space)
                |> List.map f

        Origin ->
            []


dimension : VectorSpace a -> Int
dimension space =
    case space of
        Span { basis } ->
            basis
                |> List.head
                |> Maybe.map Vector.dimension
                |> Maybe.withDefault 0

        Origin ->
            0


toVector : Field a -> VectorSpace a -> List a -> Vector a
toVector field space coefficients =
    let
        zero =
            Vector.zero field (dimension space)
    in
    case space of
        Span { basis } ->
            zip coefficients basis
                |> List.map (uncurry (Vector.scale field))
                |> List.foldl (Vector.add field) zero

        Origin ->
            zero
