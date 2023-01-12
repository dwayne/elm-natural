module Test.Lib exposing (suite)

import Expect
import Lib
import Natural
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lib"
        [ factSuite
        ]


factSuite : Test
factSuite =
    describe "fact"
        [ test "100!" <|
            \_ ->
                let
                    oneHundredFactorial =
                        Natural.fromString "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000"
                in
                Natural.fromInt 100
                    |> Maybe.map Lib.fact
                    |> Expect.equal oneHundredFactorial
        ]
