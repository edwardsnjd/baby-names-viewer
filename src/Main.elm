module Main exposing (main)

import Browser
import Page.ListNames as ListNames


main : Program () ListNames.Model ListNames.Msg
main =
    Browser.element
        { init = ListNames.init
        , view = ListNames.view
        , update = ListNames.update
        , subscriptions = \_ -> Sub.none
        }
