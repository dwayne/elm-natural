module Lib exposing
    ( fact
    , fromInt
    )

import Natural exposing (Natural)


fromInt : Int -> Natural
fromInt =
    Natural.fromInt >> Maybe.withDefault Natural.zero


fact : Natural -> Natural
fact n =
    if Natural.isZero n then
        Natural.one

    else
        Natural.mul n (fact (Natural.sub n Natural.one))
