module Algebra.Matrix exposing (Matrix, Operation(..), fromList, rowEchelon)

import Algebra.Vector as Vector exposing (Vector)
import Array exposing (Array)
import Field exposing (Field)


type Matrix a
    = Rows (Array (Vector a))


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


type Operation a
    = Swap Int Int
    | Multiply a Int
    | Linear a Int Int


type SearchResult a
    = NotFound
    | Found Int Int a


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
