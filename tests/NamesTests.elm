module NamesTests exposing (suite)

import Expect
import Fuzz exposing (stringOfLength, stringOfLengthBetween)
import Names exposing (Filter(..), check, matchingAll)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "Names module"
        [ describe "check"
            [ describe "MinLength 4"
                [ fuzz shortNames "rejects < 4" <|
                    \str -> check lengthAtLeastFour str |> assertError "Name was too short"
                , fuzz namesOfFour "accepts == 4" <|
                    \str -> check lengthAtLeastFour str |> Expect.ok
                , fuzz longNames "accepts > 4" <|
                    \str -> check lengthAtLeastFour str |> Expect.ok
                ]
            , describe "MaxLength 4"
                [ fuzz shortNames "accepts < 4" <|
                    \str -> check lengthAtMostFour str |> Expect.ok
                , fuzz namesOfFour "accepts == 4" <|
                    \str -> check lengthAtMostFour str |> Expect.ok
                , fuzz longNames "rejects > 4" <|
                    \str -> check lengthAtMostFour str |> assertError "Name was too long"
                ]
            , describe "StartsWith A"
                [ fuzz namesStartingWithA "accepts A..." <|
                    \str -> check startsWithA str |> Expect.ok
                , fuzz namesStartingWithB "rejects B..." <|
                    \str -> check startsWithA str |> assertError "Name did not start with right prefix"
                ]
            ]
        , describe "matchingAll"
            [ fuzz shortNames "can limit max length" <|
                \str -> matchingAll [ lengthAtLeastFour ] [ str ] |> Expect.equal []
            , fuzz namesOfFour "can target length" <|
                \str -> matchingAll [ lengthAtLeastFour, lengthAtMostFour ] [ str ] |> Expect.equal [ str ]
            , fuzz longNames "can limit min length" <|
                \str -> matchingAll [ lengthAtLeastFour ] [ str ] |> Expect.equal [ str ]
            , fuzz shortNames "accepts < 4" <|
                \str -> matchingAll [ lengthAtMostFour ] [ str ] |> Expect.equal [ str ]
            , fuzz longNames "rejects > 4" <|
                \str -> matchingAll [ lengthAtMostFour ] [ str ] |> Expect.equal []
            , fuzz namesStartingWithA "accepts A..." <|
                \str -> matchingAll [ startsWithA ] [ str ] |> Expect.equal [ str ]
            , fuzz namesStartingWithB "rejects B..." <|
                \str -> matchingAll [ startsWithA ] [ str ] |> Expect.equal []
            ]
        ]



-- Utils


assertError : a -> Result a value -> Expect.Expectation
assertError expectedError actual =
    case actual of
        Err err ->
            Expect.equal err expectedError

        _ ->
            Expect.fail "Expecting error but got ok"



-- Fuzzers


shortNames : Fuzz.Fuzzer String
shortNames =
    stringOfLengthBetween 0 3


namesOfFour : Fuzz.Fuzzer String
namesOfFour =
    stringOfLength 4


longNames : Fuzz.Fuzzer String
longNames =
    stringOfLengthBetween 5 20


namesStartingWithA : Fuzz.Fuzzer String
namesStartingWithA =
    stringOfLength 4 |> Fuzz.map (String.append "A")


namesStartingWithB : Fuzz.Fuzzer String
namesStartingWithB =
    stringOfLength 4 |> Fuzz.map (String.append "B")



-- Filters


lengthAtLeastFour : Filter
lengthAtLeastFour =
    MinLength 4


lengthAtMostFour : Filter
lengthAtMostFour =
    MaxLength 4


startsWithA : Filter
startsWithA =
    StartsWith "A"
