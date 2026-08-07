module Field exposing (Field)


type alias Field a =
    { zero : a
    , one : a
    , fromInt : Int -> a
    , addition : a -> a -> a
    , negation : a -> a
    , multiplication : a -> a -> a
    , invertion : a -> Maybe a
    }
