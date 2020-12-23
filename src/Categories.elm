module Categories exposing (Category, CategoryModel, CategoryPrivate, Msg, init, update, view)

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
import Message
import Misc exposing (bootstrapIcon)



-- INIT


type alias Category =
    { id : Int
    , title : String
    }


type alias CategoryForm =
    { title : String
    }


type Operation
    = UpdateOp
    | DeleteOp
    | FetchOp


type alias CategoryPrivate =
    { edit : Maybe Int
    , form : CategoryForm
    , currentOp : Maybe Operation
    }


type alias CategoryModel m =
    { m
        | categories : List Category
        , categoryPrivate : CategoryPrivate
        , message : Message.Model
    }


init : () -> ( CategoryPrivate, Cmd Msg )
init _ =
    ( CategoryPrivate Nothing emptyForm (Just FetchOp), getAccountsCmd )


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


updateFromResult : CategoryModel m -> Result Http.Error a -> (a -> ( CategoryModel m, Cmd Msg )) -> ( CategoryModel m, Cmd Msg )
updateFromResult model res f =
    let
        oldPrivate =
            model.categoryPrivate
    in
    case res of
        Ok a ->
            f a

        Err (Http.BadBody e) ->
            ( { model | message = Message.danger e, categoryPrivate = { oldPrivate | currentOp = Nothing } }, Cmd.none )

        Err _ ->
            ( { model
                | message = Message.danger "Error!"
                , categoryPrivate =
                    { oldPrivate
                        | currentOp = Nothing
                    }
              }
            , Cmd.none
            )


type alias PrivateOp m =
    { m | currentOp : Maybe Operation }


type alias PrivateEdit m =
    { m | edit : Maybe Int }


type alias PrivateForm m =
    { m | form : CategoryForm }


update : Msg -> CategoryModel m -> ( CategoryModel m, Cmd Msg )
update msg model =
    let
        oldForm =
            model.categoryPrivate.form

        updateOp : Maybe Operation -> PrivateOp CategoryPrivate -> PrivateOp CategoryPrivate
        updateOp o p =
            { p | currentOp = o }

        updateEdit : Maybe Int -> PrivateEdit CategoryPrivate -> PrivateEdit CategoryPrivate
        updateEdit o p =
            { p | edit = o }

        updateForm : CategoryForm -> PrivateForm CategoryPrivate -> PrivateForm CategoryPrivate
        updateForm o p =
            { p | form = o }
    in
    case msg of
        GotCategories res ->
            (\a -> ( { model | categories = a, categoryPrivate = model.categoryPrivate |> updateOp Nothing }, Cmd.none ))
                |> updateFromResult model res

        GotResponseAdd res ->
            (\a ->
                ( { model
                    | message = Message.info "Added account"
                    , categories = a :: model.categories
                    , categoryPrivate = model.categoryPrivate |> updateOp Nothing |> updateEdit Nothing
                  }
                , Cmd.none
                )
            )
                |> updateFromResult model res

        GotResponseDelete res ->
            case model.categoryPrivate.edit of
                Nothing ->
                    ( model, Cmd.none )

                Just i ->
                    (\_ ->
                        ( { model
                            | message = Message.info "Deleted account"
                            , categories = removeAt i model.categories
                            , categoryPrivate = model.categoryPrivate |> updateOp Nothing |> updateEdit Nothing
                          }
                        , Cmd.none
                        )
                    )
                        |> updateFromResult model res

        GotResponseUpdate res ->
            case model.categoryPrivate.edit of
                Nothing ->
                    ( model, Cmd.none )

                Just i ->
                    (\a ->
                        ( { model
                            | message = Message.info "Updated account"
                            , categories = setAt i a model.categories
                            , categoryPrivate = model.categoryPrivate |> updateOp Nothing |> updateEdit Nothing
                          }
                        , Cmd.none
                        )
                    )
                        |> updateFromResult model res

        Add ->
            ( { model | categoryPrivate = model.categoryPrivate |> updateForm emptyForm |> updateEdit (Just -1) }, Cmd.none )

        Remove i ->
            case getAt i model.categories of
                Just category ->
                    ( { model | categoryPrivate = model.categoryPrivate |> updateOp (Just DeleteOp) }
                    , delCategoryCmd category.id
                    )

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
                    ( { model
                        | categoryPrivate =
                            model.categoryPrivate
                                |> updateEdit (Just i)
                                |> updateForm (CategoryForm e.title)
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        EditTitle v ->
            ( { model | categoryPrivate = model.categoryPrivate |> updateForm { oldForm | title = v } }, Cmd.none )

        AcceptEdit ->
            case model.categoryPrivate.edit of
                Nothing ->
                    ( model, Cmd.none )

                Just i ->
                    if not (validateForm model.categoryPrivate.form) then
                        ( model, Cmd.none )

                    else if i == -1 then
                        -- New row
                        ( { model | categoryPrivate = model.categoryPrivate |> updateOp (Just UpdateOp) }
                        , addCategoryCmd model.categoryPrivate.form
                        )

                    else
                        case getAt i model.categories of
                            Nothing ->
                                ( { model
                                    | categoryPrivate =
                                        model.categoryPrivate
                                            |> updateForm emptyForm
                                            |> updateEdit Nothing
                                  }
                                , Cmd.none
                                )

                            Just currentEntry ->
                                ( { model | categoryPrivate = model.categoryPrivate |> updateOp (Just UpdateOp) }
                                , updateCategoryCmd currentEntry.id model.categoryPrivate.form
                                )

        CancelEdit ->
            ( { model | categoryPrivate = model.categoryPrivate |> updateForm emptyForm |> updateEdit Nothing }, Cmd.none )


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


view : CategoryModel m -> Html Msg
view model =
    let
        isEditing : Int -> Maybe Int
        isEditing i =
            MExtra.filter ((==) i) model.categoryPrivate.edit

        spinnerIfOp : Operation -> Html msg -> Html msg
        spinnerIfOp o e =
            MExtra.filter ((==) o) model.categoryPrivate.currentOp
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
                , Table.td []
                    [ Input.text
                        [ Input.attrs
                            [ value model.categoryPrivate.form.title
                            , onInput EditTitle
                            , disabled (MExtra.isJust model.categoryPrivate.currentOp)
                            ]
                        ]
                    ]
                , Table.td []
                    [ spinnerIfOp UpdateOp
                        (Button.button
                            [ Button.outlinePrimary
                            , Button.attrs
                                [ disabled (MExtra.isJust model.categoryPrivate.currentOp)
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
                            , disabled (MExtra.isJust model.categoryPrivate.currentOp)
                            ]
                        ]
                        [ bootstrapIcon "x" ]
                    , spinnerIfOp DeleteOp
                        (Button.button
                            [ Button.outlinePrimary
                            , Button.attrs
                                [ Spacing.ml1
                                , onClick (Remove i)
                                , disabled (MExtra.isJust model.categoryPrivate.currentOp)
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
                        , Button.attrs [ disabled (MExtra.isJust model.categoryPrivate.currentOp), Spacing.ml1, onClick (Edit i) ]
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
        , Button.button
            [ Button.outlinePrimary
            , Button.attrs
                [ Spacing.m1
                , onClick Add
                , disabled (MExtra.isJust model.categoryPrivate.currentOp)
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
