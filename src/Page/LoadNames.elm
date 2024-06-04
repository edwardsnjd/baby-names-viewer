module Page.LoadNames exposing (Model, Msg, init, update, view)

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


femaleNames : String
femaleNames =
    "https://raw.githubusercontent.com/edwardsnjd/baby-names/main/names-female.csv"


maleNames : String
maleNames =
    "https://raw.githubusercontent.com/edwardsnjd/baby-names/main/names-male.csv"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { loadedNames = []
      , url = femaleNames
      , errorMessage = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUrl url ->
            ( { model | url = url }, Cmd.none )

        FetchUrl ->
            ( { model | loadedNames = [], errorMessage = Nothing }, getUrl model.url )

        FetchReceived (Ok names) ->
            ( { model | loadedNames = String.words names, errorMessage = Nothing }, Cmd.none )

        FetchReceived (Err error) ->
            ( { model | errorMessage = Just (buildErrorMessage error) }, Cmd.none )


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
                    [ option
                        [ value femaleNames ]
                        [ text "Female names" ]
                    , option
                        [ value maleNames ]
                        [ text "Male names" ]
                    ]
                ]
            , p []
                [ input
                    [ type_ "text"
                    , value model.url
                    , onInput UpdateUrl
                    , style "font-size" "1.5em"
                    , style "width" "100%"
                    , placeholder femaleNames
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
        , h2 [] [ text "Result:" ]
        , p [ style "color" "grey" ]
            [ (List.length model.loadedNames |> String.fromInt) ++ " names loaded." |> text ]
        , p [ style "color" "red" ]
            [ Maybe.withDefault "" model.errorMessage |> text ]
        ]
