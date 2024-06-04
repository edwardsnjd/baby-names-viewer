module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.ListNames as ListNames
import Page.LoadNames as LoadNames



-- Main page.  Follows the nested Elm architecture pattern, with an "outcome" pattern
-- to allow the child `LoadNames` to communicate a new list of names to `ListNames`.
-- See: http://sporto.github.io/elm-patterns/architecture/child-outcome.html


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { loadNames : LoadNames.Model
    , listNames : ListNames.Model
    }


type Msg
    = LoadNames LoadNames.Msg
    | ListNames ListNames.Msg


type alias Update =
    ( Model, Cmd Msg )


init : () -> Update
init _ =
    let
        ( loadNamesModel, loadNamesCmd ) =
            LoadNames.init ()

        ( listNamesModel, listNamesCmd ) =
            ListNames.init ()
    in
    ( { loadNames = loadNamesModel
      , listNames = listNamesModel
      }
    , Cmd.batch
        [ loadNamesCmd |> Cmd.map LoadNames
        , listNamesCmd |> Cmd.map ListNames
        ]
    )


update : Msg -> Model -> Update
update wrappedMsg model =
    case wrappedMsg of
        LoadNames msg ->
            let
                ( result, maybeOutcome ) =
                    loadNamesUpdate msg model
            in
            maybeOutcome
                |> Maybe.map (handleOutcome result)
                |> Maybe.withDefault result

        ListNames msg ->
            listNamesUpdate msg model


loadNamesUpdate : LoadNames.Msg -> Model -> ( Update, Maybe LoadNames.Outcome )
loadNamesUpdate msg model =
    let
        ( updated, cmd, outcome ) =
            LoadNames.update msg model.loadNames
    in
    ( ( { model | loadNames = updated }, cmd |> Cmd.map LoadNames ), outcome )


handleOutcome : Update -> LoadNames.Outcome -> Update
handleOutcome ( model, cmd ) outcome =
    case outcome of
        LoadNames.UpdatedNames names ->
            listNamesUpdateNames names model |> prependCmd cmd


listNamesUpdate : ListNames.Msg -> Model -> Update
listNamesUpdate msg model =
    let
        ( updated, cmd ) =
            ListNames.update msg model.listNames
    in
    ( { model | listNames = updated }, Cmd.map ListNames cmd )


listNamesUpdateNames : List String -> Model -> Update
listNamesUpdateNames names model =
    let
        ( updated, cmd ) =
            ListNames.updateNames names model.listNames
    in
    ( { model | listNames = updated }, Cmd.map ListNames cmd )


prependCmd : Cmd Msg -> Update -> Update
prependCmd firstCmd ( model, secondCmd ) =
    ( model, Cmd.batch [ firstCmd, secondCmd ] )



-- VIEWS


view : Model -> Html Msg
view model =
    div
        [ style "padding" "1em"
        , style "font-size" "120%"
        ]
        [ LoadNames.view model.loadNames |> Html.map LoadNames
        , ListNames.view model.listNames |> Html.map ListNames
        ]
