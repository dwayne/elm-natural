module Lib exposing
    ( fact
    , fib
    , firstNDigitsOfE
    , firstNDigitsOfPi
    )

import Natural exposing (Natural, five, four, one, ten, two, zero)


fact : Natural -> Natural
fact n =
    --
    -- Computes n!.
    --
    -- See https://en.wikipedia.org/wiki/Factorial.
    --
    if Natural.isZero n then
        one

    else
        Natural.mul n <| fact (Natural.sub n one)


fib : Int -> Natural
fib n =
    --
    -- Computes the (n+1)th Fibonacci number.
    --
    -- See https://en.wikipedia.org/wiki/Fibonacci_number.
    --
    fibHelper (max 0 n) zero one


fibHelper : Int -> Natural -> Natural -> Natural
fibHelper n a b =
    if n == 0 then
        a

    else
        fibHelper (n - 1) b (Natural.add a b)


firstNDigitsOfPi : Int -> String
firstNDigitsOfPi n =
    --
    -- Computes the first n digits of π using John Machin's formula.
    --
    -- See https://en.wikipedia.org/wiki/Machin-like_formula.
    --
    let
        twoThirtyNine =
            Natural.fromSafeInt 239

        -- n2 = n + 10
        n2 =
            Natural.add (Natural.fromSafeInt n) ten

        -- 4 * arctan(1/5)
        t1 =
            Natural.mul four (arctanOfReciprocal five n2)

        -- arctan(1/239)
        t2 =
            arctanOfReciprocal twoThirtyNine n2

        -- π/4 = 4*arctan(1/5) - arctan(1/239)
        t3 =
            Natural.sub t1 t2

        -- 3141592653...
        digits =
            Natural.mul four t3
                |> Natural.toString

        -- 3
        wholePart =
            String.left 1 digits

        -- 141592653...
        fractionalPart =
            digits
                |> String.dropLeft 1
                |> String.left (n - 1)
    in
    wholePart ++ "." ++ fractionalPart


arctanOfReciprocal : Natural -> Natural -> Natural
arctanOfReciprocal x n =
    --
    -- Computes arctan(1/x) using the Taylor series expansion for arctangent.
    --
    let
        -- 10^n
        tenToN =
            Natural.exp ten n

        -- x^2
        d2 =
            Natural.mul x x

        loop sum d i isPos =
            let
                -- 2*i + 1
                odd =
                    Natural.add (Natural.mul two i) one

                -- 10^n / ( d * (2*i + 1) )
                term =
                    tenToN |> iDivBy (Natural.mul d odd)
            in
            if Natural.isZero term then
                sum

            else
                let
                    nextSum =
                        -- This is how we handle the (-1)^i factor.
                        --
                        if isPos then
                            -- sum = sum + term
                            Natural.add sum term

                        else
                            -- sum = sum - term
                            Natural.sub sum term

                    nextD =
                        -- d = d * d2 = d * x^2
                        Natural.mul d d2

                    nextI =
                        -- i = i + 1
                        Natural.add i one
                in
                loop nextSum nextD nextI (not isPos)
    in
    loop zero x zero True


firstNDigitsOfE : Int -> String
firstNDigitsOfE n =
    --
    -- Computes the first n digits of Euler's number, e.
    --
    -- See https://en.wikipedia.org/wiki/E_(mathematical_constant).
    --
    let
        -- m = n + 10
        m =
            Natural.add (Natural.fromSafeInt n) ten

        -- 10^m
        tenToM =
            Natural.exp ten m

        loop sum denom i =
            if i |> Natural.isGreaterThan m then
                sum

            else
                let
                    term =
                        tenToM |> iDivBy denom

                    nextSum =
                        Natural.add sum term

                    nextI =
                        Natural.add i one

                    nextDenom =
                        Natural.mul denom nextI
                in
                loop nextSum nextDenom nextI

        -- 2718281828...
        digits =
            loop zero one zero
                |> Natural.toString

        -- 2
        wholePart =
            String.left 1 digits

        -- 718281828...
        fractionalPart =
            digits
                |> String.dropLeft 1
                |> String.left (n - 1)
    in
    wholePart ++ "." ++ fractionalPart


iDivBy : Natural -> Natural -> Natural
iDivBy b a =
    --
    -- Assumes b is non-zero.
    --
    Natural.divBy b a
        |> Maybe.withDefault zero
