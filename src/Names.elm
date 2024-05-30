module Names exposing (Filter(..), Name, check, matchingAll)

import Utils exposing (isOk)


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
