module Names exposing (Filter(..), Name, StringSpec(..), check, matchingAll, toFilters)


type alias Name =
    String


type alias FilterResult =
    Result Name String


type Filter
    = MinLength Int
    | MaxLength Int
    | StartsWithOneOf (List StringSpec)
    | EndsWithOneOf (List StringSpec)
    | Not Filter


type StringSpec
    = Simple String
    | Range String String



{- Parse query to list of filters -}


toFilters : String -> List Filter
toFilters query =
    query
        |> String.trim
        |> String.words
        |> List.filterMap toFilter


toFilter : String -> Maybe Filter
toFilter term =
    case String.split ":" term of
        [ k, t ] ->
            case String.toLower k of
                "min" ->
                    toMinLength t

                "max" ->
                    toMaxLength t

                "startswith" ->
                    toStartsWithOneOf t

                "endswith" ->
                    toEndsWithOneOf t

                "!min" ->
                    toMinLength t |> Maybe.map Not

                "!max" ->
                    toMaxLength t |> Maybe.map Not

                "!startswith" ->
                    toStartsWithOneOf t |> Maybe.map Not

                "!endswith" ->
                    toEndsWithOneOf t |> Maybe.map Not

                _ ->
                    Nothing

        _ ->
            Nothing


toMinLength : String -> Maybe Filter
toMinLength term =
    String.toInt term
        |> Maybe.map MinLength


toMaxLength : String -> Maybe Filter
toMaxLength term =
    String.toInt term
        |> Maybe.map MaxLength


toStartsWithOneOf : String -> Maybe Filter
toStartsWithOneOf term =
    let
        specs =
            String.split "," term |> List.filterMap toSpec
    in
    if List.length specs == 0 then
        Nothing

    else
        Just (StartsWithOneOf specs)


toEndsWithOneOf : String -> Maybe Filter
toEndsWithOneOf term =
    let
        specs =
            String.split "," term |> List.filterMap toSpec
    in
    if List.length specs == 0 then
        Nothing

    else
        Just (EndsWithOneOf specs)


toSpec : String -> Maybe StringSpec
toSpec str =
    case String.split "-" str of
        [ "" ] ->
            Nothing

        [ s ] ->
            Just (Simple s)

        [ "", _ ] ->
            Nothing

        [ _, "" ] ->
            Nothing

        [ from, to ] ->
            Just (Range from to)

        _ ->
            Nothing



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

        EndsWithOneOf specs ->
            let
                -- NOTE: This is accepts ANY of the given suffixes
                anySuffixMatches =
                    List.any
                        (\spec ->
                            case spec of
                                Simple str ->
                                    String.endsWith str name

                                Range from to ->
                                    let
                                        reversedName =
                                            String.reverse name

                                        reversedFrom =
                                            String.reverse from

                                        reversedTo =
                                            String.reverse to
                                    in
                                    -- NOTE: Can't simply compare because longer strings sort after prefix
                                    reversedFrom <= reversedName && String.slice 0 (String.length reversedTo) reversedName <= reversedTo
                        )
                        specs
            in
            if anySuffixMatches then
                Ok name

            else
                Err "Name did not end with right suffix"

        Not childFilter ->
            case check childFilter name of
                Ok _ ->
                    Err "Removed"

                Err _ ->
                    Ok name


isOk : Result a b -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        _ ->
            False
