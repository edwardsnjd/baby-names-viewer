module NamesTests exposing (suite)

import Expect
import Fuzz exposing (stringOfLength, stringOfLengthBetween)
import Names exposing (Filter(..), StringSpec(..), check, matchingAll, toFilters)
import Test exposing (Test, describe, fuzz, test)


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
            , describe "StartsWithOneOf A"
                [ fuzz namesStartingWithA "accepts A..." <|
                    \str -> check startsWithOneOfA str |> Expect.ok
                , fuzz namesStartingWithB "rejects B..." <|
                    \str -> check startsWithOneOfA str |> assertError "Name did not start with right prefix"
                ]
            , describe "StartsWithOneOf A or C"
                [ fuzz namesStartingWithA "accepts A..." <|
                    \str -> check startsWithOneOfAorC str |> Expect.ok
                , fuzz namesStartingWithB "rejects B..." <|
                    \str -> check startsWithOneOfAorC str |> assertError "Name did not start with right prefix"
                , fuzz namesStartingWithC "accepts C..." <|
                    \str -> check startsWithOneOfAorC str |> Expect.ok
                , fuzz namesStartingWithD "rejects D..." <|
                    \str -> check startsWithOneOfAorC str |> assertError "Name did not start with right prefix"
                ]
            , describe "StartsWithOneOf A to C"
                [ fuzz namesStartingWithA "accepts A..." <|
                    \str -> check startsWithOneOfAtoC str |> Expect.ok
                , fuzz namesStartingWithB "accepts B..." <|
                    \str -> check startsWithOneOfAtoC str |> Expect.ok
                , fuzz namesStartingWithC "accepts C..." <|
                    \str -> check startsWithOneOfAtoC str |> Expect.ok
                , fuzz namesStartingWithD "rejects D..." <|
                    \str -> check startsWithOneOfAtoC str |> assertError "Name did not start with right prefix"
                ]
            , describe "EndsWithOneOf A"
                [ fuzz namesEndingWithA "accepts ...A" <|
                    \str -> check endsWithOneOfA str |> Expect.ok
                , fuzz namesEndingWithB "rejects ...B" <|
                    \str -> check endsWithOneOfA str |> assertError "Name did not end with right suffix"
                ]
            , describe "EndsWithOneOf A or C"
                [ fuzz namesEndingWithA "accepts ...A" <|
                    \str -> check endsWithOneOfAorC str |> Expect.ok
                , fuzz namesEndingWithB "rejects ...B" <|
                    \str -> check endsWithOneOfAorC str |> assertError "Name did not end with right suffix"
                , fuzz namesEndingWithC "accepts ...C" <|
                    \str -> check endsWithOneOfAorC str |> Expect.ok
                , fuzz namesEndingWithD "rejects ...D" <|
                    \str -> check endsWithOneOfAorC str |> assertError "Name did not end with right suffix"
                ]
            , describe "EndsWithOneOf A to C"
                [ fuzz namesEndingWithA "accepts ...A" <|
                    \str -> check endsWithOneOfAtoC str |> Expect.ok
                , fuzz namesEndingWithB "accepts ...B" <|
                    \str -> check endsWithOneOfAtoC str |> Expect.ok
                , fuzz namesEndingWithC "accepts ...C" <|
                    \str -> check endsWithOneOfAtoC str |> Expect.ok
                , fuzz namesEndingWithD "rejects ...D" <|
                    \str -> check endsWithOneOfAtoC str |> assertError "Name did not end with right suffix"
                ]
            , describe "Not MinLength 4"
                [ fuzz shortNames "accepts < 4" <|
                    \str -> check notLengthAtLeastFour str |> Expect.ok
                , fuzz namesOfFour "rejects == 4" <|
                    \str -> check notLengthAtLeastFour str |> assertError "Removed"
                , fuzz longNames "rejects > 4" <|
                    \str -> check notLengthAtLeastFour str |> assertError "Removed"
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
                \str -> matchingAll [ startsWithOneOfA ] [ str ] |> Expect.equal [ str ]
            , fuzz namesStartingWithB "rejects B..." <|
                \str -> matchingAll [ startsWithOneOfA ] [ str ] |> Expect.equal []
            , fuzz namesEndingWithB "rejects ...B" <|
                \str -> matchingAll [ endsWithOneOfA ] [ str ] |> Expect.equal []
            ]
        , describe "matchingAll fuzzing"
            [ fuzz rangeOfNames "can limit min length" <|
                \strs -> matchingAll [ lengthAtLeastFour ] strs |> assertAll (isAtLeast 4)
            , fuzz rangeOfNames "can limit max length" <|
                \strs -> matchingAll [ lengthAtMostFour ] strs |> assertAll (isAtMost 4)
            , fuzz rangeOfNames "can target length" <|
                \strs -> matchingAll [ lengthAtLeastFour, lengthAtMostFour ] strs |> assertAll (isExactly 4)
            ]
        , describe "toFilters"
            [ describe "whitespace"
                [ test "empty string maps to empty list" <|
                    \_ -> toFilters "" |> Expect.equal []
                , test "space maps to empty list" <|
                    \_ -> toFilters " " |> Expect.equal []
                , test "spaces maps to empty list" <|
                    \_ -> toFilters "     " |> Expect.equal []
                ]
            , describe "min:???"
                [ test "empty" <|
                    \_ -> toFilters "min:" |> Expect.equal []
                , test "space after prefix" <|
                    \_ -> toFilters "min: " |> Expect.equal []
                , test "space before term" <|
                    \_ -> toFilters "min: 3" |> Expect.equal []
                , test "letter" <|
                    \_ -> toFilters "min:A" |> Expect.equal []
                , test "floating point" <|
                    \_ -> toFilters "min:1.2" |> Expect.equal []
                , test "integer" <|
                    \_ -> toFilters "min:42" |> Expect.equal [ MinLength 42 ]
                , test "trailing space" <|
                    \_ -> toFilters "min:42 " |> Expect.equal [ MinLength 42 ]
                , test "ignore case of key" <|
                    \_ -> toFilters "MiN:42 " |> Expect.equal [ MinLength 42 ]
                ]
            , describe "max:???"
                [ test "empty" <|
                    \_ -> toFilters "max:" |> Expect.equal []
                , test "space after prefix" <|
                    \_ -> toFilters "max: " |> Expect.equal []
                , test "space before term" <|
                    \_ -> toFilters "max: 3" |> Expect.equal []
                , test "letter" <|
                    \_ -> toFilters "max:A" |> Expect.equal []
                , test "floating point" <|
                    \_ -> toFilters "max:1.2" |> Expect.equal []
                , test "integer" <|
                    \_ -> toFilters "max:42" |> Expect.equal [ MaxLength 42 ]
                , test "trailing space" <|
                    \_ -> toFilters "max:42 " |> Expect.equal [ MaxLength 42 ]
                , test "ignore case of key" <|
                    \_ -> toFilters "MaX:42 " |> Expect.equal [ MaxLength 42 ]
                ]
            , describe "startswith:???"
                [ test "empty" <|
                    \_ -> toFilters "startswith:" |> Expect.equal []
                , test "space after prefix" <|
                    \_ -> toFilters "startswith: " |> Expect.equal []
                , test "space before term" <|
                    \_ -> toFilters "startswith: A" |> Expect.equal []
                , test "single character" <|
                    \_ -> toFilters "startswith:A" |> Expect.equal [ StartsWithOneOf [ Simple "A" ] ]
                , test "trailing space" <|
                    \_ -> toFilters "startswith:A " |> Expect.equal [ StartsWithOneOf [ Simple "A" ] ]
                , test "pair" <|
                    \_ -> toFilters "startswith:A,C" |> Expect.equal [ StartsWithOneOf [ Simple "A", Simple "C" ] ]
                , test "range" <|
                    \_ -> toFilters "startswith:A-C" |> Expect.equal [ StartsWithOneOf [ Range "A" "C" ] ]
                , test "ignore case of key" <|
                    \_ -> toFilters "StArTswITH:A-C" |> Expect.equal [ StartsWithOneOf [ Range "A" "C" ] ]
                ]
            , describe "endswith:???"
                [ test "empty" <|
                    \_ -> toFilters "endswith:" |> Expect.equal []
                , test "space after prefix" <|
                    \_ -> toFilters "endswith: " |> Expect.equal []
                , test "space before term" <|
                    \_ -> toFilters "endswith: A" |> Expect.equal []
                , test "single character" <|
                    \_ -> toFilters "endswith:A" |> Expect.equal [ EndsWithOneOf [ Simple "A" ] ]
                , test "trailing space" <|
                    \_ -> toFilters "endswith:A " |> Expect.equal [ EndsWithOneOf [ Simple "A" ] ]
                , test "pair" <|
                    \_ -> toFilters "endswith:A,C" |> Expect.equal [ EndsWithOneOf [ Simple "A", Simple "C" ] ]
                , test "range" <|
                    \_ -> toFilters "endswith:A-C" |> Expect.equal [ EndsWithOneOf [ Range "A" "C" ] ]
                , test "ignore case of key" <|
                    \_ -> toFilters "EnDsWiTh:A-C" |> Expect.equal [ EndsWithOneOf [ Range "A" "C" ] ]
                ]
            , describe "!???:???"
                [ test "!max" <|
                    \_ -> toFilters "!max:42" |> Expect.equal [ Not (MaxLength 42) ]
                , test "!min" <|
                    \_ -> toFilters "!min:42" |> Expect.equal [ Not (MinLength 42) ]
                , test "!startswith" <|
                    \_ -> toFilters "!startswith:A" |> Expect.equal [ Not (StartsWithOneOf [ Simple "A" ]) ]
                , test "!endswith" <|
                    \_ -> toFilters "!endswith:A" |> Expect.equal [ Not (EndsWithOneOf [ Simple "A" ]) ]
                ]
            , describe "combinations"
                [ test "min, max, then start" <|
                    \_ -> toFilters "min:4 max:10 startswith:A" |> Expect.equal [ MinLength 4, MaxLength 10, StartsWithOneOf [ Simple "A" ] ]
                , test "start, min, then max" <|
                    \_ -> toFilters "startswith:A min:4 max:10" |> Expect.equal [ StartsWithOneOf [ Simple "A" ], MinLength 4, MaxLength 10 ]
                ]
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


assertAll2 : (a -> Bool) -> String -> List a -> Expect.Expectation
assertAll2 predicate msg values =
    let
        failures =
            List.filter (\v -> predicate v |> not) values
    in
    if List.length failures == 0 then
        Expect.pass

    else
        Expect.fail msg



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


namesStartingWith : String -> Fuzz.Fuzzer String
namesStartingWith prefix =
    stringOfLength 4 |> Fuzz.map (String.append prefix)


namesEndingWith : String -> Fuzz.Fuzzer String
namesEndingWith suffix =
    stringOfLength 4 |> Fuzz.map (\str -> String.append str suffix)


namesStartingWithA : Fuzz.Fuzzer String
namesStartingWithA =
    namesStartingWith "A"


namesStartingWithB : Fuzz.Fuzzer String
namesStartingWithB =
    namesStartingWith "B"


namesStartingWithC : Fuzz.Fuzzer String
namesStartingWithC =
    namesStartingWith "C"


namesStartingWithD : Fuzz.Fuzzer String
namesStartingWithD =
    namesStartingWith "D"


namesEndingWithA : Fuzz.Fuzzer String
namesEndingWithA =
    namesEndingWith "A"


namesEndingWithB : Fuzz.Fuzzer String
namesEndingWithB =
    namesEndingWith "B"


namesEndingWithC : Fuzz.Fuzzer String
namesEndingWithC =
    namesEndingWith "C"


namesEndingWithD : Fuzz.Fuzzer String
namesEndingWithD =
    namesEndingWith "D"


rangeOfNames : Fuzz.Fuzzer (List String)
rangeOfNames =
    Fuzz.list (Fuzz.stringOfLengthBetween 0 20)



-- Filters


lengthAtLeastFour : Filter
lengthAtLeastFour =
    MinLength 4


lengthAtMostFour : Filter
lengthAtMostFour =
    MaxLength 4


startsWithOneOfA : Filter
startsWithOneOfA =
    StartsWithOneOf [ Simple "A" ]


startsWithOneOfAorC : Filter
startsWithOneOfAorC =
    StartsWithOneOf [ Simple "A", Simple "C" ]


startsWithOneOfAtoC : Filter
startsWithOneOfAtoC =
    StartsWithOneOf [ Range "A" "C" ]


endsWithOneOfA : Filter
endsWithOneOfA =
    EndsWithOneOf [ Simple "A" ]


endsWithOneOfAorC : Filter
endsWithOneOfAorC =
    EndsWithOneOf [ Simple "A", Simple "C" ]


endsWithOneOfAtoC : Filter
endsWithOneOfAtoC =
    EndsWithOneOf [ Range "A" "C" ]


notLengthAtLeastFour : Filter
notLengthAtLeastFour =
    Not (MinLength 4)
