module Algebra.VectorSpace exposing (VectorSpace, add, contains, empty, equals, intersection, span)

import Algebra.Vector as Vector exposing (Vector)
import Field exposing (Field)
import General exposing (swap, uncurry, zip)


type VectorSpace a
    = Span { basis : List (Vector a) }
    | Origin


span : Field a -> List (Vector a) -> VectorSpace a
span field basis =
    List.foldl (add field) empty basis


empty : VectorSpace a
empty =
    Origin


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


intersection : Field a -> VectorSpace a -> VectorSpace a -> VectorSpace a
intersection field u v =
    case ( u, v ) of
        ( Span left, Span _ ) ->
            left.basis
                |> List.map (swap (projection field) v)
                |> span field

        _ ->
            Origin


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
