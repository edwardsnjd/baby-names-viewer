module Page.LoadNames exposing (Model, Msg, Outcome(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


type alias Model =
    { loadedNames : List String
    , url : String
    , errorMessage : Maybe String
    }


type Msg
    = UpdateUrl String
    | FetchUrl
    | FetchReceived (Result Http.Error String)


type Outcome
    = UpdatedNames (List String)


type alias NameGroup =
    { female : String, male : String }


nameUrls : { us : NameGroup, welsh : NameGroup, scottish : NameGroup }
nameUrls =
    { scottish =
        { female = "https://raw.githubusercontent.com/edwardsnjd/baby-names/main/scottish-names-female.csv"
        , male = "https://raw.githubusercontent.com/edwardsnjd/baby-names/main/scottish-names-male.csv"
        }
    , us =
        { female = "https://raw.githubusercontent.com/edwardsnjd/baby-names/main/us-names-female.csv"
        , male = "https://raw.githubusercontent.com/edwardsnjd/baby-names/main/us-names-male.csv"
        }
    , welsh =
        { female = "https://raw.githubusercontent.com/edwardsnjd/baby-names/main/welsh-names-female.csv"
        , male = "https://raw.githubusercontent.com/edwardsnjd/baby-names/main/welsh-names-male.csv"
        }
    }


init : () -> ( Model, Cmd Msg, Maybe Outcome )
init _ =
    ( { loadedNames = []
      , url = nameUrls.us.female
      , errorMessage = Nothing
      }
    , Cmd.none
    , Nothing
    )



{- Special update function that optionally returns an outcome for other modules to respond to. -}


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Outcome )
update msg model =
    case msg of
        UpdateUrl url ->
            ( { model | url = url }, Cmd.none, Nothing )

        FetchUrl ->
            ( { model | loadedNames = [], errorMessage = Nothing }, getUrl model.url, Just (UpdatedNames []) )

        FetchReceived (Ok str) ->
            let
                names =
                    String.words str
            in
            ( { model | loadedNames = names, errorMessage = Nothing }, Cmd.none, Just (UpdatedNames names) )

        FetchReceived (Err error) ->
            ( { model | errorMessage = Just (buildErrorMessage error) }, Cmd.none, Nothing )


getUrl : String -> Cmd Msg
getUrl url =
    Http.get
        { url = url
        , expect = Http.expectString FetchReceived
        }


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



-- VIEWS


view : Model -> Html Msg
view model =
    section []
        [ h2 [] [ text "Names file:" ]
        , Html.form
            [ onSubmit FetchUrl ]
            [ p []
                [ select
                    [ onInput UpdateUrl
                    , value model.url
                    , style "font-size" "1.5em"
                    , style "width" "100%"
                    ]
                    [ optgroup
                        [ attribute "label" "Female" ]
                        [ viewUrlOption model nameUrls.scottish.female "Scottish female names"
                        , viewUrlOption model nameUrls.us.female "US female names"
                        , viewUrlOption model nameUrls.welsh.female "Welsh female names"
                        ]
                    , optgroup
                        [ attribute "label" "Male" ]
                        [ viewUrlOption model nameUrls.scottish.male "Scottish male names"
                        , viewUrlOption model nameUrls.us.male "US male names"
                        , viewUrlOption model nameUrls.welsh.male "Welsh male names"
                        ]
                    ]
                ]
            , p []
                [ input
                    [ type_ "text"
                    , value model.url
                    , onInput UpdateUrl
                    , style "font-size" "1.5em"
                    , style "width" "100%"
                    , placeholder nameUrls.us.female
                    ]
                    []
                ]
            , p []
                [ button
                    [ type_ "submit"
                    , style "font-size" "1.5em"
                    , style "width" "100%"
                    ]
                    [ text "Load" ]
                ]
            ]
        , p [ style "color" "grey" ]
            [ (List.length model.loadedNames |> String.fromInt) ++ " names loaded." |> text ]
        , p [ style "color" "red" ]
            [ Maybe.withDefault "" model.errorMessage |> text ]
        ]


viewUrlOption : Model -> String -> String -> Html msg
viewUrlOption model url name =
    option
        [ value url, selected (model.url == url) ]
        [ text name ]
