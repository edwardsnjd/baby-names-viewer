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
        , describe "matchingAll fuzzing"
            [ fuzz rangeOfNames "can limit min length" <|
                \strs -> matchingAll [ lengthAtLeastFour ] strs |> assertAll (isAtLeast 4)
            , fuzz rangeOfNames "can limit max length" <|
                \strs -> matchingAll [ lengthAtMostFour ] strs |> assertAll (isAtMost 4)
            , fuzz rangeOfNames "can target length" <|
                \strs -> matchingAll [ lengthAtLeastFour, lengthAtMostFour ] strs |> assertAll (isExactly 4)
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


assertAll : (a -> Result String Bool) -> List a -> Expect.Expectation
assertAll predicate values =
    let
        failures =
            List.map predicate values |> List.filterMap toError
    in
    if List.length failures == 0 then
        Expect.pass

    else
        Expect.fail (String.join "\n" failures)



-- Misc


toError : Result a value -> Maybe a
toError result =
    case result of
        Err err ->
            Just err

        _ ->
            Nothing



-- Predicates


isAtLeast : Int -> String -> Result String Bool
isAtLeast length =
    buildCheck
        (\str -> String.length str >= length)
        (\str -> "Was not longer than " ++ String.fromInt length ++ ": '" ++ str ++ "'")


isAtMost : Int -> String -> Result String Bool
isAtMost length =
    buildCheck
        (\str -> String.length str <= length)
        (\str -> "Was longer than " ++ String.fromInt length ++ ": '" ++ str ++ "'")


isExactly : Int -> String -> Result String Bool
isExactly length =
    buildCheck
        (\str -> String.length str == length)
        (\str -> "Length was not exactly " ++ String.fromInt length ++ ": '" ++ str ++ "'")


buildCheck : (c -> Bool) -> (c -> a) -> c -> Result a Bool
buildCheck pred toErr str =
    if pred str then
        Ok True

    else
        Err (toErr str)



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


rangeOfNames : Fuzz.Fuzzer (List String)
rangeOfNames =
    Fuzz.list Fuzz.string



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
