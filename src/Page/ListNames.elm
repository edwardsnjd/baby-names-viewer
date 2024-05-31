module Page.ListNames exposing (Model, Msg, init, update, view)

import BoysNames exposing (all)
import Html exposing (..)
import Html.Attributes exposing (..)
import Names exposing (Filter(..), Name, matchingAll)


type alias Model =
    { all : List Name
    , matching : List Name
    , filters : List Filter
    }


type Msg
    = Unit


init : () -> ( Model, Cmd Msg )
init _ =
    let
        filters =
            [ MinLength 3, MaxLength 6, StartsWith "X" ]
    in
    ( { all = all
      , filters = filters
      , matching = matchingAll filters all
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ div [] [ "Filters: " ++ (List.length model.filters |> String.fromInt) |> text ]
        , div [] [ "Names: " ++ (List.length model.matching |> String.fromInt) ++ " of " ++ (List.length model.all |> String.fromInt) |> text ]
        , textarea [] [ String.join "\n" model.matching |> text ]
        ]
