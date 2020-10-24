module Categories exposing (Model, Msg, init, update, view)

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
import Svg
import Svg.Attributes as Svgattr



-- INIT


type alias Category =
    { id : Int
    , title : String
    }


type alias CategoryForm =
    { title : String
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
    { categories : List Category
    , edit : Maybe Int
    , form : CategoryForm
    , message : Message
    , currentOp : Maybe Operation
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] Nothing emptyForm noMessage (Just FetchOp), getAccountsCmd )


noMessage : Message
noMessage =
    { level = Alert.info, visibility = Alert.closed, text = "" }


emptyForm : CategoryForm
emptyForm =
    CategoryForm ""


getAccountsCmd =
    Http.get
        { url = "http://localhost:8000/categories/"
        , expect = Http.expectJson GotCategories (D.list categoryParser)
        }


addCategoryCmd : CategoryForm -> Cmd Msg
addCategoryCmd form =
    Http.post
        { url = "http://localhost:8000/categories/"
        , expect = Http.expectJson GotResponseAdd categoryParser
        , body = Http.jsonBody (categoryEncoder Nothing form)
        }


delCategoryCmd : Int -> Cmd Msg
delCategoryCmd id =
    Http.request
        { method = "DELETE"
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = "http://localhost:8000/categories/" ++ String.fromInt id ++ "/"
        , expect = Http.expectWhatever GotResponseDelete
        }


updateCategoryCmd : Int -> CategoryForm -> Cmd Msg
updateCategoryCmd id form =
    Http.request
        { method = "PUT"
        , headers = []
        , body = Http.jsonBody (categoryEncoder (Just id) form)
        , timeout = Nothing
        , tracker = Nothing
        , url = "http://localhost:8000/categories/" ++ String.fromInt id ++ "/"
        , expect = Http.expectJson GotResponseUpdate categoryParser
        }


categoryParser : D.Decoder Category
categoryParser =
    D.map2 Category
        (D.field "id" D.int)
        (D.field "title" D.string)


categoryEncoder : Maybe Int -> CategoryForm -> E.Value
categoryEncoder id a =
    E.object
        [ ( "id", Maybe.map E.int id |> Maybe.withDefault E.null )
        , ( "title", E.string a.title )
        ]



-- UPDATE


type Msg
    = GotCategories (Result Http.Error (List Category))
    | GotResponseAdd (Result Http.Error Category)
    | GotResponseDelete (Result Http.Error ())
    | GotResponseUpdate (Result Http.Error Category)
    | Add
    | Remove Int
    | Edit Int
    | EditTitle String
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
        GotCategories res ->
            case res of
                Ok a ->
                    ( { model | categories = a, currentOp = Nothing }, Cmd.none )

                Err (Http.BadBody e) ->
                    ( { model | message = Message Alert.danger e Alert.shown, currentOp = Nothing }, Cmd.none )

                Err _ ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown, currentOp = Nothing }, Cmd.none )

        GotResponseAdd res ->
            case res of
                Ok a ->
                    ( { model | message = Message Alert.info "Added account" Alert.shown, categories = a :: model.categories, currentOp = Nothing, edit = Nothing }, Cmd.none )

                Err (Http.BadBody e) ->
                    ( { model | message = Message Alert.danger e Alert.shown, currentOp = Nothing }, Cmd.none )

                Err _ ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown, currentOp = Nothing }, Cmd.none )

        GotResponseDelete res ->
            case ( res, model.edit ) of
                ( _, Nothing ) ->
                    ( model, Cmd.none )

                ( Ok (), Just i ) ->
                    ( { model | message = Message Alert.info "Deleted account" Alert.shown, categories = removeAt i model.categories, currentOp = Nothing, edit = Nothing }, Cmd.none )

                ( Err _, Just i ) ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown, currentOp = Nothing }, Cmd.none )

        GotResponseUpdate res ->
            case ( res, model.edit ) of
                ( _, Nothing ) ->
                    ( model, Cmd.none )

                ( Ok a, Just i ) ->
                    ( { model | message = Message Alert.info "Updated account" Alert.shown, categories = setAt i a model.categories, currentOp = Nothing, edit = Nothing }, Cmd.none )

                ( Err (Http.BadBody e), Just i ) ->
                    ( { model | message = Message Alert.danger e Alert.shown, currentOp = Nothing }, Cmd.none )

                ( Err _, Just i ) ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown, currentOp = Nothing }, Cmd.none )

        Add ->
            ( { model | form = emptyForm, edit = Just -1 }, Cmd.none )

        Remove i ->
            case getAt i model.categories of
                Just category ->
                    ( { model | currentOp = Just DeleteOp }, delCategoryCmd category.id )

                Nothing ->
                    ( model, Cmd.none )

        Edit i ->
            let
                entry : Maybe Category
                entry =
                    getAt i model.categories
            in
            case entry of
                Just e ->
                    ( { model | edit = Just i, form = CategoryForm e.title }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTitle v ->
            ( { model | form = { oldForm | title = v } }, Cmd.none )

        AcceptEdit ->
            case model.edit of
                Nothing ->
                    ( model, Cmd.none )

                Just i ->
                    if not (validateForm model.form) then
                        ( model, Cmd.none )

                    else if i == -1 then
                        -- New row
                        ( { model | currentOp = Just UpdateOp }, addCategoryCmd model.form )

                    else
                        case getAt i model.categories of
                            Nothing ->
                                ( { model | form = emptyForm, edit = Nothing }, Cmd.none )

                            Just currentEntry ->
                                ( { model | currentOp = Just UpdateOp }, updateCategoryCmd currentEntry.id model.form )

        CancelEdit ->
            ( { model | form = emptyForm, edit = Nothing }, Cmd.none )

        AlertMsg m ->
            let
                oldMessage =
                    model.message
            in
            ( { model | message = { oldMessage | visibility = m } }, Cmd.none )


validateForm : CategoryForm -> Bool
validateForm form =
    let
        isEmpty =
            String.trim >> String.isEmpty
    in
    if isEmpty form.title then
        False

    else
        True



-- VIEW


bootstrapIcon : String -> Html Msg
bootstrapIcon name =
    Svg.svg
        [ Svgattr.class "bi", Svgattr.fill "currentColor", Svgattr.height "1.5rem", Svgattr.width "1.5rem" ]
        [ Svg.use [ Svgattr.xlinkHref ("/bootstrap-icons-1.0.0/bootstrap-icons.svg#" ++ name) ] [] ]


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

        editRow : Category -> Int -> Table.Row Msg
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
                , Table.td [] [ Input.text [ Input.attrs [ value model.form.title, onInput EditTitle, disabled (MExtra.isJust model.currentOp) ] ] ]
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

        displayRow : Category -> Int -> Table.Row Msg
        displayRow a i =
            Table.tr []
                [ Table.td [] [ text (String.fromInt a.id) ]
                , Table.td [] [ text a.title ]
                , Table.td []
                    [ Button.button
                        [ Button.outlinePrimary
                        , Button.attrs [ disabled (MExtra.isJust model.currentOp), Spacing.ml1, onClick (Edit i) ]
                        ]
                        [ bootstrapIcon "pencil" ]
                    ]
                ]

        toRow : Int -> Category -> Table.Row Msg
        toRow i a =
            isEditing i |> Maybe.map (editRow a) |> Maybe.withDefault (displayRow a i)

        addRow : Maybe (Table.Row Msg)
        addRow =
            isEditing -1 |> Maybe.map (editRow (Category -1 ""))
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
                , Table.th [] [ text "Title" ]
                ]
            , Table.tbody [] (MExtra.cons addRow (List.indexedMap toRow model.categories))
            )
        ]
