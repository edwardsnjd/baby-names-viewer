module Names exposing (Filter(..), Name, StringSpec(..), check, matchingAll, toFilters)

import Utils exposing (isOk)


type alias Name =
    String


type alias FilterResult =
    Result Name String


type Filter
    = MinLength Int
    | MaxLength Int
    | StartsWith String
    | StartsWithOneOf (List StringSpec)


type StringSpec
    = Simple String
    | Range String String



{- Parse query to list of filters -}


toFilters : String -> List (Result String Filter)
toFilters query =
    let
        trimmed =
            String.trim query
    in
    case String.length trimmed of
        0 ->
            []

        _ ->
            [ Ok (StartsWithOneOf [ Simple trimmed ]) ]



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

        StartsWithOneOf specs ->
            let
                -- NOTE: This is accepts ANY of the given prefixes
                anyPrefixMatches =
                    List.any
                        (\spec ->
                            case spec of
                                Simple str ->
                                    String.startsWith str name

                                Range from to ->
                                    -- NOTE: Can't simply compare because longer strings sort after prefix
                                    from <= name && String.slice 0 (String.length to) name <= to
                        )
                        specs
            in
            if anyPrefixMatches then
                Ok name

            else
                Err "Name did not start with right prefix"
