module Page.ListNames exposing (Model, Msg, init, update, view)

import BoysNames exposing (all)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Names exposing (Filter(..), Name, StringSpec(..), matchingAll, toFilters)


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
                , style "font-size" "2em"
                ]
                []
            ]
        , div [] [ text "Filters: " ]
        , ul [] (List.map viewFilter model.filters)
        , div []
            [ "Names: " ++ (List.length model.matching |> String.fromInt) ++ " of " ++ (List.length model.all |> String.fromInt) |> text ]
        , textarea [ style "height" "50vh" ]
            [ String.join "\n" model.matching |> text ]
        ]


viewFilter : Filter -> Html Msg
viewFilter filter =
    let
        label =
            case filter of
                MinLength min ->
                    "Min length: " ++ String.fromInt min

                MaxLength max ->
                    "Max length: " ++ String.fromInt max

                StartsWithOneOf specs ->
                    let
                        toLabel spec =
                            case spec of
                                Simple str ->
                                    str

                                Range from to ->
                                    from ++ " to " ++ to

                        specLabels =
                            List.map toLabel specs
                    in
                    "Starts with one of: " ++ String.join ", OR " specLabels
    in
    li [] [ text label ]
