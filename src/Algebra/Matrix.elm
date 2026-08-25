module Algebra.Matrix exposing
    ( Matrix, Operation(..)
    , fromList, identity
    , transpose
    , kernel, rowEchelon
    )

{-| This module provides a Matrix type.


## Type

@docs Matrix, Operation


## Creation

@docs fromList, identity


## Operation

@docs transpose


## System of equation

@docs kernel, rowEchelon

-}

import Algebra.Vector as Vector exposing (Vector)
import Algebra.VectorSpace as VectorSpace exposing (VectorSpace)
import Array exposing (Array)
import Field exposing (Field)
import General exposing (swivel)


{-| The Matrix type.

This type is parameterized over its elements.

-}
type Matrix a
    = Rows (Array (Vector a))


{-| Create a Matrix from a list of vectors.
-}
fromList : List (Vector a) -> Matrix a
fromList rows =
    Rows (Array.fromList rows)


index : Int -> Matrix a -> Maybe (Vector a)
index row (Rows rows) =
    Array.get row rows


element : Int -> Int -> Matrix a -> Maybe a
element row column matrix =
    matrix
        |> index row
        |> Maybe.andThen (Vector.index column)


rowCount : Matrix a -> Int
rowCount (Rows vs) =
    Array.length vs


columnCount : Matrix a -> Int
columnCount (Rows vs) =
    Array.get 0 vs
        |> Maybe.map Vector.dimension
        |> Maybe.withDefault 0


{-| Type of elementary operations that can be performed on a Matrix.
-}
type Operation a
    = Swap Int Int
    | Multiply a Int
    | Linear a Int Int


type SearchResult a
    = NotFound
    | Found Int Int a


{-| Return the row echelon form of a matrix.
-}
rowEchelon : Field a -> Matrix a -> ( Matrix a, List (Operation a), List Int )
rowEchelon field matrix =
    let
        firstAfterRowInColumn : (a -> Bool) -> Int -> Int -> Matrix a -> SearchResult a
        firstAfterRowInColumn predicate startRow column m =
            let
                find : Int -> SearchResult a
                find row =
                    if row < rowCount m then
                        let
                            v =
                                element row column m
                                    |> Maybe.withDefault field.zero
                        in
                        if predicate v then
                            Found row column v

                        else
                            find (row + 1)

                    else
                        NotFound
            in
            find startRow

        reduce : List (Operation a) -> Int -> Int -> Matrix a -> ( Matrix a, List (Operation a) )
        reduce acc skipRow currentColumn start =
            let
                reduceFrom : List (Operation a) -> Int -> Matrix a -> ( Matrix a, List (Operation a) )
                reduceFrom ac row m =
                    if row < rowCount m then
                        if row == skipRow then
                            reduceFrom ac (row + 1) m

                        else
                            let
                                c =
                                    element row currentColumn m
                                        |> Maybe.map field.negation
                                        |> Maybe.withDefault field.zero
                            in
                            if not (c == field.zero) then
                                reduceFrom (Linear c skipRow row :: ac) (row + 1) (linear field c skipRow row m)

                            else
                                reduceFrom ac (row + 1) m

                    else
                        ( m, ac )
            in
            reduceFrom acc 0 start

        go : List (Operation a) -> List Int -> Int -> Int -> Matrix a -> ( Matrix a, List (Operation a), List Int )
        go acc pivots currentRow currentColumn m =
            if currentColumn < columnCount m then
                case firstAfterRowInColumn (\c -> not (c == field.zero)) currentRow currentColumn m of
                    Found r c v ->
                        if currentRow < r then
                            go (Swap currentRow r :: acc) pivots currentRow currentColumn (swap field r currentRow m)

                        else if not (v == field.one) then
                            let
                                v_ =
                                    v
                                        |> field.invertion
                                        |> Maybe.withDefault field.one
                            in
                            go (Multiply v_ r :: acc) pivots currentRow currentColumn (multiplyRow field v_ r m)

                        else
                            let
                                ( m_, acc_ ) =
                                    reduce acc r c m
                            in
                            go acc_ (c :: pivots) (r + 1) (c + 1) m_

                    NotFound ->
                        go acc pivots currentRow (currentColumn + 1) m

            else
                ( m, List.reverse acc, List.reverse pivots )
    in
    go [] [] 0 0 matrix


swap : Field a -> Int -> Int -> Matrix a -> Matrix a
swap field i j ((Rows vs) as matrix) =
    let
        columns =
            columnCount matrix

        atI =
            Array.get i vs
                |> Maybe.withDefault (Vector.zero field columns)

        atJ =
            Array.get j vs
                |> Maybe.withDefault (Vector.zero field columns)
    in
    vs
        |> Array.set i atJ
        |> Array.set j atI
        |> Rows


multiplyRow : Field a -> a -> Int -> Matrix a -> Matrix a
multiplyRow field v row ((Rows rows) as matrix) =
    let
        columns =
            columnCount matrix

        scaled =
            rows
                |> Array.get row
                |> Maybe.map (Vector.scale field v)
                |> Maybe.withDefault (Vector.zero field columns)
    in
    rows
        |> Array.set row scaled
        |> Rows


linear : Field a -> a -> Int -> Int -> Matrix a -> Matrix a
linear field v from to ((Rows rows) as matrix) =
    let
        columns =
            columnCount matrix

        zero =
            Vector.zero field columns

        addend =
            rows
                |> Array.get from
                |> Maybe.map (Vector.scale field v)
                |> Maybe.withDefault zero

        result =
            rows
                |> Array.get to
                |> Maybe.map (Vector.add field addend)
                |> Maybe.withDefault zero
    in
    rows
        |> Array.set to result
        |> Rows


{-| The identity matrix of a certain dimension.
-}
identity : Field a -> Int -> Matrix a
identity field n =
    let
        base : Int -> Vector a
        base i =
            field.one
                :: List.repeat (n - 1) field.zero
                |> swivel (negate i)
                |> Vector.fromList
    in
    Array.initialize n base
        |> Rows


{-| Returns the transpose of a Matrix.
-}
transpose : Field a -> Matrix a -> Matrix a
transpose field matrix =
    let
        row : Int -> Vector a
        row c =
            List.range 0 (rowCount matrix - 1)
                |> List.map (\r -> element r c matrix |> Maybe.withDefault field.zero)
                |> Vector.fromList
    in
    Array.initialize (columnCount matrix) row
        |> Rows


{-| Determine the kernel of a matrix.
-}
kernel : Field a -> Matrix a -> VectorSpace a
kernel field matrix =
    let
        ( _, operations, pivots ) =
            rowEchelon field matrix

        numberOfRows =
            rowCount matrix

        id =
            identity field numberOfRows

        basis =
            operations
                |> List.foldl (perform field) id
                |> selectRows (List.length pivots) numberOfRows
    in
    VectorSpace.span field basis


perform : Field a -> Operation a -> Matrix a -> Matrix a
perform field operation matrix =
    case operation of
        Swap i j ->
            swap field i j matrix

        Multiply v i ->
            multiplyRow field v i matrix

        Linear v i j ->
            linear field v i j matrix


selectRows : Int -> Int -> Matrix a -> List (Vector a)
selectRows i j (Rows rows) =
    rows
        |> Array.slice i j
        |> Array.toList
