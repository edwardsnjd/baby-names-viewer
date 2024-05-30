module Utils exposing (isOk)

{- Test if the given result is ok or a failure. -}


isOk : Result a b -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        _ ->
            False
