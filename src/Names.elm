module Names exposing (Name, Filter, matchingAll, suite)

import Expect
import Fuzz exposing (stringOfLength, stringOfLengthBetween)
import Test exposing (describe, fuzz)


type alias Name =
    String


type alias FilterResult =
    Result Name String


type Filter
    = MinLength Int
    | MaxLength Int
    | StartsWith String



{- Find all names that pass all the given filters. -}


matchingAll : List Filter -> List Name -> List Name
matchingAll filters names =
    List.filter (\name -> matchesAll filters name) names



{- Test if the given result is ok or a failure. -}


isOk : FilterResult -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        _ ->
            False



-- Misc


matches : Filter -> Name -> Bool
matches filter name =
    check filter name |> isOk


matchesAll : List Filter -> Name -> Bool
matchesAll filters name =
    List.all (\filter -> matches filter name) filters


check : Filter -> Name -> FilterResult
check filter name =
    case filter of
        MinLength length ->
            if String.length name >= length then
                Ok name

            else
                Err "Name was too short"

        MaxLength length ->
            if String.length name <= length then
                Ok name

            else
                Err "Name was too long"

        StartsWith str ->
            if String.startsWith str name then
                Ok name

            else
                Err "Name did not start with right prefix"



-- Tests


suite : Test.Test
suite =
    let
        assertError expectedError actual =
            case actual of
                Err err ->
                    Expect.equal err expectedError

                _ ->
                    Expect.fail "Expecting error but got ok"

        shortNames =
            stringOfLengthBetween 0 3

        namesOfFour =
            stringOfLength 4

        longNames =
            stringOfLengthBetween 5 20

        namesStartingWithA =
            stringOfLength 4 |> Fuzz.map (String.append "A")

        namesStartingWithB =
            stringOfLength 4 |> Fuzz.map (String.append "B")

        lengthAtLeastFour =
            MinLength 4

        lengthAtMostFour =
            MaxLength 4

        startsWithA =
            StartsWith "A"
    in
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
