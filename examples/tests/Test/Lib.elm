module Test.Lib exposing (suite)

import Expect
import Lib
import Natural as N
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lib"
        [ factSuite
        , fibSuite
        , firstNDigitsOfPiSuite
        , firstNDigitsOfESuite
        ]


factSuite : Test
factSuite =
    describe "fact"
        [ test "100!" <|
            \_ ->
                Lib.fact (N.fromSafeInt 100)
                    |> N.toString
                    |> Expect.equal "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000"
        ]


fibSuite : Test
fibSuite =
    describe "fib"
        [ test "1000th" <|
            \_ ->
                Lib.fib 999
                    |> N.toString
                    |> Expect.equal "26863810024485359386146727202142923967616609318986952340123175997617981700247881689338369654483356564191827856161443356312976673642210350324634850410377680367334151172899169723197082763985615764450078474174626"
        ]


firstNDigitsOfPiSuite : Test
firstNDigitsOfPiSuite =
    describe "firstNDigitsOfPi"
        [ test "the first 100 digits of π" <|
            \_ ->
                Lib.firstNDigitsOfPi 100
                    |> Expect.equal "3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067"
        ]


firstNDigitsOfESuite : Test
firstNDigitsOfESuite =
    describe "firstNDigitsOfE"
        [ test "the first 100 digits of e" <|
            \_ ->
                Lib.firstNDigitsOfE 100
                    |> Expect.equal "2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427"
        ]
