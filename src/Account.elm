module Account exposing (Model, Msg, init, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Html exposing (Html, button, div, input, option, select, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import List.Extra exposing (getAt, removeAt, setAt)
import Maybe.Extra as MExtra
import Misc exposing (bootstrapIcon)



-- INIT


type alias Account =
    { id : Int
    , name : String
    , backend_id : String
    , backend_type : String
    }


type alias AccountForm =
    { name : String
    , backend_id : String
    , backend_type : String
    }


type alias Message =
    { level : Alert.Config Msg -> Alert.Config Msg
    , text : String
    , visibility : Alert.Visibility
    }


type Operation
    = UpdateOp
    | DeleteOp
    | FetchOp


type alias Model =
    { accounts : List Account
    , edit : Maybe Int
    , form : AccountForm
    , message : Message
    , currentOp : Maybe Operation
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] Nothing emptyForm noMessage (Just FetchOp), getAccountsCmd )


noMessage : Message
noMessage =
    { level = Alert.info, visibility = Alert.closed, text = "" }


emptyForm : AccountForm
emptyForm =
    AccountForm "" "" ""


getAccountsCmd =
    Http.get
        { url = "http://localhost:8000/accounts/"
        , expect = Http.expectJson GotAccounts (D.list accountParser)
        }


addAccountCmd : AccountForm -> Cmd Msg
addAccountCmd form =
    Http.post
        { url = "http://localhost:8000/accounts/"
        , expect = Http.expectJson GotResponseAdd accountParser
        , body = Http.jsonBody (accountEncoder Nothing form)
        }


delAccountCmd : Int -> Cmd Msg
delAccountCmd id =
    Http.request
        { method = "DELETE"
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = "http://localhost:8000/accounts/" ++ String.fromInt id ++ "/"
        , expect = Http.expectWhatever GotResponseDelete
        }


updateAccountCmd : Int -> AccountForm -> Cmd Msg
updateAccountCmd id form =
    Http.request
        { method = "PUT"
        , headers = []
        , body = Http.jsonBody (accountEncoder (Just id) form)
        , timeout = Nothing
        , tracker = Nothing
        , url = "http://localhost:8000/accounts/" ++ String.fromInt id ++ "/"
        , expect = Http.expectJson GotResponseUpdate accountParser
        }


accountParser : D.Decoder Account
accountParser =
    D.map4 Account
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.field "backend_id" D.string)
        (D.field "backend_type" D.string)


accountEncoder : Maybe Int -> AccountForm -> E.Value
accountEncoder id a =
    E.object
        [ ( "id", Maybe.map E.int id |> Maybe.withDefault E.null )
        , ( "name", E.string a.name )
        , ( "backend_id", E.string a.backend_id )
        , ( "backend_type", E.string a.backend_type )
        ]



-- UPDATE


type Msg
    = GotAccounts (Result Http.Error (List Account))
    | GotResponseAdd (Result Http.Error Account)
    | GotResponseDelete (Result Http.Error ())
    | GotResponseUpdate (Result Http.Error Account)
    | Add
    | Remove Int
    | Edit Int
    | EditName String
    | EditBackendId String
    | EditBackendType String
    | AcceptEdit
    | CancelEdit
    | AlertMsg Alert.Visibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldForm =
            model.form
    in
    case msg of
        GotAccounts res ->
            case res of
                Ok a ->
                    ( { model | accounts = a, currentOp = Nothing }, Cmd.none )

                Err (Http.BadBody e) ->
                    ( { model | message = Message Alert.danger e Alert.shown, currentOp = Nothing }, Cmd.none )

                Err _ ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown, currentOp = Nothing }, Cmd.none )

        GotResponseAdd res ->
            case res of
                Ok a ->
                    ( { model | message = Message Alert.info "Added account" Alert.shown, accounts = a :: model.accounts, currentOp = Nothing, edit = Nothing }, Cmd.none )

                Err (Http.BadBody e) ->
                    ( { model | message = Message Alert.danger e Alert.shown, currentOp = Nothing }, Cmd.none )

                Err _ ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown, currentOp = Nothing }, Cmd.none )

        GotResponseDelete res ->
            case ( res, model.edit ) of
                ( _, Nothing ) ->
                    ( model, Cmd.none )

                ( Ok (), Just i ) ->
                    ( { model | message = Message Alert.info "Deleted account" Alert.shown, accounts = removeAt i model.accounts, currentOp = Nothing, edit = Nothing }, Cmd.none )

                ( Err _, Just i ) ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown, currentOp = Nothing }, Cmd.none )

        GotResponseUpdate res ->
            case ( res, model.edit ) of
                ( _, Nothing ) ->
                    ( model, Cmd.none )

                ( Ok a, Just i ) ->
                    ( { model | message = Message Alert.info "Updated account" Alert.shown, accounts = setAt i a model.accounts, currentOp = Nothing, edit = Nothing }, Cmd.none )

                ( Err (Http.BadBody e), Just i ) ->
                    ( { model | message = Message Alert.danger e Alert.shown, currentOp = Nothing }, Cmd.none )

                ( Err _, Just i ) ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown, currentOp = Nothing }, Cmd.none )

        Add ->
            ( { model | form = emptyForm, edit = Just -1 }, Cmd.none )

        Remove i ->
            case getAt i model.accounts of
                Just account ->
                    ( { model | currentOp = Just DeleteOp }, delAccountCmd account.id )

                Nothing ->
                    ( model, Cmd.none )

        Edit i ->
            let
                entry : Maybe Account
                entry =
                    getAt i model.accounts
            in
            case entry of
                Just e ->
                    ( { model | edit = Just i, form = AccountForm e.name e.backend_id e.backend_type }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditName v ->
            ( { model | form = { oldForm | name = v } }, Cmd.none )

        EditBackendId v ->
            ( { model | form = { oldForm | backend_id = v } }, Cmd.none )

        EditBackendType v ->
            ( { model | form = { oldForm | backend_type = v } }, Cmd.none )

        AcceptEdit ->
            case model.edit of
                Nothing ->
                    ( model, Cmd.none )

                Just i ->
                    if not (validateForm model.form) then
                        ( model, Cmd.none )

                    else if i == -1 then
                        -- New row
                        ( { model | currentOp = Just UpdateOp }, addAccountCmd model.form )

                    else
                        case getAt i model.accounts of
                            Nothing ->
                                ( { model | form = emptyForm, edit = Nothing }, Cmd.none )

                            Just currentEntry ->
                                ( { model | currentOp = Just UpdateOp }, updateAccountCmd currentEntry.id model.form )

        CancelEdit ->
            ( { model | form = emptyForm, edit = Nothing }, Cmd.none )

        AlertMsg m ->
            let
                oldMessage =
                    model.message
            in
            ( { model | message = { oldMessage | visibility = m } }, Cmd.none )


validateForm : AccountForm -> Bool
validateForm form =
    let
        isEmpty =
            String.trim >> String.isEmpty
    in
    if isEmpty form.name || isEmpty form.backend_id || isEmpty form.backend_type then
        False

    else
        True



-- VIEW


view : Model -> Html Msg
view model =
    let
        isEditing : Int -> Maybe Int
        isEditing i =
            MExtra.filter ((==) i) model.edit

        spinnerIfOp : Operation -> Html msg -> Html msg
        spinnerIfOp o e =
            MExtra.filter ((==) o) model.currentOp
                |> Maybe.map
                    (\_ ->
                        Button.button [ Button.outlinePrimary, Button.attrs [] ]
                            [ Spinner.spinner [ Spinner.small, Spinner.attrs [ Spacing.m1 ] ] []
                            ]
                    )
                |> Maybe.withDefault e

        editRow : Account -> Int -> Table.Row Msg
        editRow a i =
            Table.tr []
                [ Table.td []
                    [ text
                        (if a.id >= 0 then
                            String.fromInt a.id

                         else
                            -- Unsaved new rows don't have an id
                            ""
                        )
                    ]
                , Table.td [] [ Input.text [ Input.attrs [ value model.form.name, onInput EditName, disabled (MExtra.isJust model.currentOp) ] ] ]
                , Table.td [] [ Input.text [ Input.attrs [ value model.form.backend_type, onInput EditBackendType, disabled (MExtra.isJust model.currentOp) ] ] ]
                , Table.td [] [ Input.text [ Input.attrs [ value model.form.backend_id, onInput EditBackendId, disabled (MExtra.isJust model.currentOp) ] ] ]
                , Table.td []
                    [ spinnerIfOp UpdateOp
                        (Button.button
                            [ Button.outlinePrimary
                            , Button.attrs
                                [ disabled (MExtra.isJust model.currentOp)
                                , onClick AcceptEdit
                                ]
                            ]
                            [ bootstrapIcon "check" ]
                        )
                    , Button.button
                        [ Button.outlinePrimary
                        , Button.attrs
                            [ Spacing.ml1
                            , onClick CancelEdit
                            , disabled (MExtra.isJust model.currentOp)
                            ]
                        ]
                        [ bootstrapIcon "x" ]
                    , spinnerIfOp DeleteOp
                        (Button.button
                            [ Button.outlinePrimary
                            , Button.attrs
                                [ Spacing.ml1
                                , onClick (Remove i)
                                , disabled (MExtra.isJust model.currentOp)
                                ]
                            ]
                            [ bootstrapIcon "trash" ]
                        )
                    ]
                ]

        displayRow : Account -> Int -> Table.Row Msg
        displayRow a i =
            Table.tr []
                [ Table.td [] [ text (String.fromInt a.id) ]
                , Table.td [] [ text a.name ]
                , Table.td [] [ text a.backend_type ]
                , Table.td [] [ text a.backend_id ]
                , Table.td []
                    [ Button.button
                        [ Button.outlinePrimary
                        , Button.attrs [ disabled (MExtra.isJust model.currentOp), Spacing.ml1, onClick (Edit i) ]
                        ]
                        [ bootstrapIcon "pencil" ]
                    ]
                ]

        toRow : Int -> Account -> Table.Row Msg
        toRow i a =
            isEditing i |> Maybe.map (editRow a) |> Maybe.withDefault (displayRow a i)

        addRow : Maybe (Table.Row Msg)
        addRow =
            isEditing -1 |> Maybe.map (editRow (Account -1 "" "" ""))
    in
    div []
        [ CDN.stylesheet
        , Alert.config
            |> Alert.dismissable AlertMsg
            |> model.message.level
            |> Alert.children [ text model.message.text ]
            |> Alert.view model.message.visibility
        , Button.button
            [ Button.outlinePrimary
            , Button.attrs
                [ Spacing.m1
                , onClick Add
                , disabled (MExtra.isJust model.currentOp)
                ]
            ]
            [ bootstrapIcon "plus" ]
        , Table.simpleTable
            ( Table.simpleThead
                [ Table.th [] [ text "ID" ]
                , Table.th [] [ text "Name" ]
                , Table.th [] [ text "Account Type" ]
                , Table.th [] [ text "Account ID" ]
                ]
            , Table.tbody [] (MExtra.cons addRow (List.indexedMap toRow model.accounts))
            )
        ]
