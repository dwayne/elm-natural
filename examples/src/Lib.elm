module Lib exposing
    ( fact
    , fib
    , firstNDigitsOfE
    , firstNDigitsOfPi
    )

import Natural as N exposing (Natural)


fact : Natural -> Natural
fact n =
    --
    -- Compute n!.
    --
    -- See https://en.wikipedia.org/wiki/Factorial.
    --
    if N.isZero n then
        N.one

    else
        N.mul n <| fact (N.sub n N.one)


fib : Int -> Natural
fib n =
    --
    -- Compute the (n+1)th Fibonacci number.
    --
    -- See https://en.wikipedia.org/wiki/Fibonacci_number.
    --
    fibHelper (max 0 n) N.zero N.one


fibHelper : Int -> Natural -> Natural -> Natural
fibHelper n a b =
    if n == 0 then
        a

    else
        fibHelper (n - 1) b (N.add a b)


firstNDigitsOfPi : Int -> String
firstNDigitsOfPi n =
    --
    -- Compute the first n digits of π using John Machin's formula.
    --
    -- See https://en.wikipedia.org/wiki/Machin-like_formula.
    --
    let
        twoThirtyNine =
            N.fromSafeInt 239

        -- n2 = n + 10
        n2 =
            N.add (N.fromSafeInt n) N.ten

        -- 4 * arctan(1/5)
        t1 =
            N.mul N.four (arctanOfReciprocal N.five n2)

        -- arctan(1/239)
        t2 =
            arctanOfReciprocal twoThirtyNine n2

        -- π/4 = 4*arctan(1/5) - arctan(1/239)
        t3 =
            N.sub t1 t2

        -- 3141592653...
        digits =
            N.toString <| N.mul N.four t3

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
    -- Compute arctan(1/x) using the Taylor series expansion for arctangent.
    --
    let
        -- 10^n
        tenToN =
            N.exp N.ten n

        -- x^2
        d2 =
            N.mul x x

        loop sum d i isPos =
            let
                -- 2*i + 1
                odd =
                    N.add (N.mul N.two i) N.one

                -- 10^n / ( d * (2*i + 1) )
                term =
                    tenToN |> iDivBy (N.mul d odd)
            in
            if N.isZero term then
                sum

            else
                let
                    nextSum =
                        -- This is how we handle the (-1)^i factor.
                        --
                        if isPos then
                            -- sum = sum + term
                            N.add sum term

                        else
                            -- sum = sum - term
                            N.sub sum term

                    nextD =
                        -- d = d * d2 = d * x^2
                        N.mul d d2

                    nextI =
                        -- i = i + 1
                        N.add i N.one
                in
                loop nextSum nextD nextI (not isPos)
    in
    loop N.zero x N.zero True


firstNDigitsOfE : Int -> String
firstNDigitsOfE n =
    --
    -- Compute the first n digits of Euler's number, e.
    --
    -- See https://en.wikipedia.org/wiki/E_(mathematical_constant).
    --
    let
        -- m = n + 10
        m =
            N.add (N.fromSafeInt n) N.ten

        -- 10^m
        tenToM =
            N.exp N.ten m

        loop sum denom i =
            if i |> N.isGreaterThan m then
                sum

            else
                let
                    term =
                        tenToM |> iDivBy denom

                    nextSum =
                        N.add sum term

                    nextI =
                        N.add i N.one

                    nextDenom =
                        N.mul denom nextI
                in
                loop nextSum nextDenom nextI

        -- 2718281828...
        digits =
            N.toString <| loop N.zero N.one N.zero

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
    N.divBy b a
        |> Maybe.withDefault N.zero
