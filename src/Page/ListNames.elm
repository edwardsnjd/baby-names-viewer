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
    div
        [ style "padding" "1em"
        , style "font-size" "120%"
        ]
        [ h2 [] [ text "Query:" ]
        , p []
            [ input
                [ type_ "text"
                , value model.query
                , onInput UpdateQuery
                , style "font-size" "1.5em"
                , placeholder "startswith:A min:4 max:10"
                ]
                []
            ]
        , details [ style "color" "grey" ]
            [ summary [] [ text "Supported filters" ]
            , dl
                []
                ([ ( "startswith:{prefix}", "Starts with {prefix}" )
                 , ( "startswith:{prefix1},{prefix2},...", "Starts with either {prefix1} or {prefix2} or any other prefixes" )
                 , ( "startswith:{from}-{to},...", "Starts between {from} and {to}, or any other supplied prefixes" )
                 , ( "min:{length}", "Has at least {length} letters" )
                 , ( "max:{length}", "Has at most {length} letters" )
                 ]
                    |> List.concatMap
                        (\t ->
                            [ dt [] [ code [] [ text (Tuple.first t) ] ]
                            , dd [] [ text (Tuple.second t) ]
                            ]
                        )
                )
            ]
        , h2 [] [ text "Filters (from query):" ]
        , if List.length model.filters == 0 then
            p [ style "color" "grey" ] [ text "None (type a query)" ]

          else
            ul [] (List.map viewFilter model.filters)
        , h2 [] [ text "Matching names:" ]
        , p [ style "color" "grey" ]
            [ (List.length model.matching |> String.fromInt) ++ " of " ++ (List.length model.all |> String.fromInt) |> text ]
        , textarea
            [ style "height" "50vh"
            , style "width" "100%"
            , style "font-size" "0.9em"
            , style "overflow" "scroll"
            ]
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
