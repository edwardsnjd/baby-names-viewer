module Page.ListNames exposing (Model, Msg, init, update, updateNames, view)

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
    | UpdateNames (List Name)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { all = []
      , query = ""
      , filters = []
      , matching = matchingAll [] []
      }
    , Cmd.none
    )



{- Special update function that other modules to update this without exposing Msg constructors. -}


updateNames : List Name -> Model -> ( Model, Cmd Msg )
updateNames names model =
    update (UpdateNames names) model


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
                        , matching = matchingAll filters model.all
                    }

                UpdateNames all ->
                    { model
                        | all = all
                        , matching = matchingAll model.filters all
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
                , style "width" "100%"
                , placeholder "startswith:A !endswith:ie,y min:4 max:10"
                ]
                []
            , p [ style "color" "grey" ]
                [ text "The matched names will filter as you type.  If it's not working, check the gotchas!" ]
            ]
        , details [ style "color" "grey" ]
            [ summary [] [ text "ℹ️  Supported filters and gotchas" ]
            , dl
                []
                ([ ( "startswith:{prefix}", "Starts with {prefix}" )
                 , ( "startswith:{prefix1},{prefix2},...", "Starts with either {prefix1} or {prefix2} or any other prefixes" )
                 , ( "startswith:{from}-{to},...", "Starts between {from} and {to}, or any other supplied prefixes" )
                 , ( "endswith:{suffix}", "Ends with {suffix}" )
                 , ( "endswith:{suffix1},{suffix2},...", "Ends with either {suffix1} or {suffix2} or any other suffixes" )
                 , ( "endswith:{from}-{to},...", "Ends between {from} and {to}, or any other supplied suffixes" )
                 , ( "min:{length}", "Has at least {length} letters" )
                 , ( "max:{length}", "Has at most {length} letters" )
                 , ( "!{key}:{value}", "Does not match whatever the key and value would match" )
                 ]
                    |> List.concatMap
                        (\t ->
                            [ dt [] [ code [] [ text (Tuple.first t) ] ]
                            , dd [] [ text (Tuple.second t) ]
                            ]
                        )
                )
            , p [] [ text "Gotchas:" ]
            , ul []
                [ li []
                    [ text "No spaces between the search key, the colon, and the search value e.g. "
                    , span [ style "color" "green" ] [ text "`min:4`" ]
                    , text " is good but "
                    , span [ style "color" "red" ] [ text "`min : 4`" ]
                    , text " will not work."
                    ]
                , li []
                    [ text "Name prefixes are case sensitive e.g. "
                    , span [ style "color" "green" ] [ text "`startswith:B`" ]
                    , text " is good but "
                    , span [ style "color" "red" ] [ text "`startswith:b`" ]
                    , text " will not work."
                    ]
                , li []
                    [ text "Use the `!` when it's easier to say what you don't want instead of what you do want e.g."
                    , span [ style "color" "green" ] [ text "`!startswith:J,W,Y`" ]
                    , text " is good because it's shorter than "
                    , span [ style "color" "orange" ] [ text "`startswith:A-I,K-V,X,Z`" ]
                    , text "."
                    ]
                ]
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
        label f =
            case f of
                Not nested ->
                    "NOT " ++ (label nested)

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

                EndsWithOneOf specs ->
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
                    "Ends with one of: " ++ String.join ", OR " specLabels
    in
    li [] [ text (label filter) ]
