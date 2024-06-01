module Page.ListNames exposing (Model, Msg, init, update, view)

import BoysNames exposing (all)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Names exposing (Filter(..), Name, matchingAll)


type alias Model =
    { all : List Name
    , query : String
    , matching : List Name
    , filters : List Filter
    }


type Msg
    = UpdateQuery String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { all = all
      , query = ""
      , filters = []
      , matching = matchingAll [] all
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updated =
            case msg of
                UpdateQuery query ->
                    let
                        filters =
                            toFilters query
                    in
                    { model
                        | query = query
                        , filters = filters
                        , matching = matchingAll filters all
                    }
    in
    ( updated, Cmd.none )


toFilters : String -> List Filter
toFilters query =
    [ StartsWith query ]



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Query: "
            , input
                [ type_ "text"
                , value model.query
                , onInput UpdateQuery
                ]
                []
            ]
        , div [] [ "Filters: " ++ (List.length model.filters |> String.fromInt) |> text ]
        , div [] [ "Names: " ++ (List.length model.matching |> String.fromInt) ++ " of " ++ (List.length model.all |> String.fromInt) |> text ]
        , textarea [] [ String.join "\n" model.matching |> text ]
        ]
