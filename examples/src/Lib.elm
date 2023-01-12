module Lib exposing
    ( fact
    , fib
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


fib : Int -> Natural
fib n =
    fibHelper (max 0 n) Natural.zero Natural.one


fibHelper : Int -> Natural -> Natural -> Natural
fibHelper n a b =
    if n == 0 then
        a

    else
        fibHelper (n - 1) b (Natural.add a b)
