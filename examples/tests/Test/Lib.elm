module Test.Lib exposing (suite)

import Expect
import Lib
import Natural
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lib"
        [ factSuite
        , fibSuite
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


fibSuite : Test
fibSuite =
    describe "fib"
        [ test "1000th" <|
            \_ ->
                Lib.fib 999
                    |> Natural.toString
                    |> Expect.equal "26863810024485359386146727202142923967616609318986952340123175997617981700247881689338369654483356564191827856161443356312976673642210350324634850410377680367334151172899169723197082763985615764450078474174626"
        ]
