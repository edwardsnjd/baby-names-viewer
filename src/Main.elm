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
    = LoadNamesMsg LoadNames.Msg
    | ListNamesMsg ListNames.Msg


type alias Update =
    ( Model, Cmd Msg )


type alias ListNamesUpdate =
    ( ListNames.Model, Cmd ListNames.Msg )


type alias LoadNamesUpdate =
    ( LoadNames.Model, Cmd LoadNames.Msg, Maybe LoadNames.Outcome )



-- Build Elm update


init : () -> Update
init _ =
    mergeAll
        (LoadNames.init ())
        (ListNames.init ())


update : Msg -> Model -> Update
update wrappedMsg model =
    case wrappedMsg of
        LoadNamesMsg msg ->
            loadNamesUpdate ( model, Cmd.none ) msg

        ListNamesMsg msg ->
            listNamesUpdate ( model, Cmd.none ) msg


loadNamesUpdate : Update -> LoadNames.Msg -> Update
loadNamesUpdate ( model, cmd ) msg =
    LoadNames.update msg model.loadNames
        |> mergeLoadNames ( model, cmd )


handleOutcome : Maybe LoadNames.Outcome -> Update -> Update
handleOutcome outcome result =
    case outcome of
        Just (LoadNames.UpdatedNames names) ->
            listNamesUpdateNames result names

        _ ->
            result


listNamesUpdate : Update -> ListNames.Msg -> Update
listNamesUpdate ( model, cmd ) msg =
    ListNames.update msg model.listNames
        |> mergeListNames ( model, cmd )


listNamesUpdateNames : Update -> List String -> Update
listNamesUpdateNames ( model, cmd ) names =
    ListNames.updateNames names model.listNames
        |> mergeListNames ( model, cmd )



-- Build update response from child responses


mergeLoadNames : Update -> LoadNamesUpdate -> Update
mergeLoadNames ( model, cmd ) ( childModel, childCmd, outcome ) =
    ( { model | loadNames = childModel }
    , Cmd.batch [ cmd, Cmd.map LoadNamesMsg childCmd ]
    )
        |> handleOutcome outcome


mergeListNames : Update -> ListNamesUpdate -> Update
mergeListNames ( model, cmd ) ( childModel, childCmd ) =
    ( { model | listNames = childModel }
    , Cmd.batch [ cmd, Cmd.map ListNamesMsg childCmd ]
    )


mergeAll : LoadNamesUpdate -> ListNamesUpdate -> Update
mergeAll ( loadNamesModel, loadNamesCmd, _ ) ( listNamesModel, listNamesCmd ) =
    ( { loadNames = loadNamesModel, listNames = listNamesModel }
    , Cmd.batch [ Cmd.map LoadNamesMsg loadNamesCmd, Cmd.map ListNamesMsg listNamesCmd ]
    )



-- VIEWS


view : Model -> Html Msg
view model =
    div
        [ style "padding" "1em"
        , style "font-size" "120%"
        ]
        [ LoadNames.view model.loadNames |> Html.map LoadNamesMsg
        , ListNames.view model.listNames |> Html.map ListNamesMsg
        ]
