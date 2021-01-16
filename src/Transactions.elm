port module Transactions exposing (Msg, Transaction, TransactionPrivate, categoryTableView, init, transactionParser, update, view)

import Account exposing (Account)
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Categories exposing (Category)
import Date exposing (Date, fromIsoString, toIsoString)
import DatePicker exposing (defaultSettings)
import Debug
import Dict exposing (Dict)
import EditableTable
import Html exposing (Html, button, div, h2, input, option, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Decode.Pipeline as Dp
import Json.Encode as E
import List.Extra exposing (gatherEqualsBy, gatherWith, getAt, removeAt, setAt, unique)
import Maybe.Extra exposing (cons, isJust, isNothing, join)
import Message
import Misc exposing (bootstrapIcon)
import Task


port toClipboard : String -> Cmd msg



-- MODEL


type alias Currency =
    String


currencies : List Currency
currencies =
    [ "ILS", "USD", "EUR" ]


type alias Transaction =
    { transaction_date : Date
    , bill_date : Date
    , original_amount : Float
    , billed_amount : Float
    , currency : Currency
    , description : String
    , from_account : String
    , to_account : String
    , category : String
    }


type FloatField
    = FloatField (Maybe Float) String


type IntField
    = IntField (Maybe Int) String


type alias TransactionForm =
    { transaction_date : Maybe Date
    , bill_date : Maybe Date
    , original_amount : FloatField
    , billed_amount : FloatField
    , currency : Currency
    , description : String
    , from_account : String
    , to_account : String
    , category : String
    }


type alias FetchModel =
    { selected_backend : String
    , month : IntField
    , year : IntField
    , username : String
    , password : String
    , open : Bool
    }


type TableType
    = ExpenseTable
    | IncomeTable
    | InterTable


type EditOp
    = Exist Int
    | New TableType


type alias TransactionPrivate =
    { edit : Maybe EditOp
    , form : TransactionForm
    , billDatePicker : DatePicker.DatePicker
    , transactionDatePicker : DatePicker.DatePicker
    , fetch : FetchModel
    }


type alias TransactionModel m =
    { m
        | transactions : List Transaction
        , transactionPrivate : TransactionPrivate
        , message : Message.Model
        , accounts : List Account
        , categories : List Category
    }


type alias ModelTable model =
    { model
        | transactions : List Transaction
        , accounts : List Account
        , categories : List Category
        , transactionPrivate : TransactionPrivate
    }


init : () -> ( TransactionPrivate, Cmd Msg )
init m =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( TransactionPrivate Nothing emptyForm datePicker datePicker resetFetch
    , Cmd.batch
        [ Cmd.map (ToDatePicker AllDate) datePickerFx
        , fetchDateCmd
        ]
    )


billDateSettings : DatePicker.Settings
billDateSettings =
    { defaultSettings | placeholder = "Bill Date", inputClassList = [ ( "form-control", True ) ] }


transactionDateSettings : DatePicker.Settings
transactionDateSettings =
    { defaultSettings | placeholder = "Transaction Date", inputClassList = [ ( "form-control", True ) ] }


emptyForm : TransactionForm
emptyForm =
    { transaction_date = Nothing
    , bill_date = Nothing
    , original_amount = FloatField (Just 0) ""
    , billed_amount = FloatField (Just 0) ""
    , currency = "ILS"
    , description = ""
    , from_account = ""
    , to_account = ""
    , category = ""
    }


resetFetch : FetchModel
resetFetch =
    { selected_backend = ""
    , month = IntField Nothing "0"
    , year = IntField Nothing "0"
    , username = ""
    , password = ""
    , open = False
    }



-- UPDATE


type DatePickerType
    = BillDate
    | TransactionDate
    | AllDate


type Msg
    = Add TableType
    | Remove Int
    | StartEditRow Int
    | SaveEditRow
    | CancelEditRow
    | EditTransAmount String
    | EditBillAmount String
    | EditDescription String
    | ChangeCategory (Maybe Int) String
    | ChangeCurrency Currency
    | ChangeFromAccount String
    | ChangeToAccount String
    | ToDatePicker DatePickerType DatePicker.Msg
    | GotTransactions (Result Http.Error (List Transaction))
    | FetchEditYear String
    | FetchEditMonth String
    | FetchEditUsername String
    | FetchEditPassword String
    | FetchChangeBackend String
    | FetchGotDate Date
    | FetchGo
    | ClickSaveTable (Transaction -> Bool)
    | FinishSaveTable (Transaction -> Bool) (Result Http.Error ())
    | CopyCategories


fetchDateCmd =
    Date.today |> Task.perform FetchGotDate


update : Msg -> TransactionModel m -> ( TransactionModel m, Cmd Msg )
update msg model =
    let
        oldPrivate =
            model.transactionPrivate

        oldForm =
            oldPrivate.form

        oldFetch =
            oldPrivate.fetch
    in
    case msg of
        Add table ->
            ( { model | transactionPrivate = { oldPrivate | form = emptyForm, edit = Just (New table) } }
            , Cmd.none
            )

        Remove index ->
            ( { model
                | transactionPrivate = { oldPrivate | edit = Nothing, form = emptyForm }
                , transactions = removeAt index model.transactions
              }
            , Cmd.none
            )

        StartEditRow n ->
            case getAt n model.transactions of
                Nothing ->
                    ( model, Cmd.none )

                Just r ->
                    ( { model | transactionPrivate = { oldPrivate | edit = Just (Exist n), form = toForm r } }
                    , Cmd.none
                    )

        CancelEditRow ->
            ( { model | transactionPrivate = { oldPrivate | edit = Nothing, form = emptyForm } }, Cmd.none )

        SaveEditRow ->
            case createTransaction model.transactionPrivate.form of
                Just t ->
                    case model.transactionPrivate.edit of
                        Nothing ->
                            ( model, Cmd.none )

                        Just e ->
                            case e of
                                Exist i ->
                                    ( { model
                                        | transactionPrivate = { oldPrivate | edit = Nothing, form = emptyForm }
                                        , transactions = setAt i t model.transactions
                                      }
                                    , Cmd.none
                                    )

                                New _ ->
                                    ( { model
                                        | transactionPrivate = { oldPrivate | edit = Nothing, form = emptyForm }
                                        , transactions = t :: model.transactions
                                      }
                                    , Cmd.none
                                    )

                Nothing ->
                    ( model, Cmd.none )

        EditTransAmount a ->
            ( { model
                | transactionPrivate = { oldPrivate | form = { oldForm | original_amount = FloatField (String.toFloat a) a } }
              }
            , Cmd.none
            )

        EditBillAmount a ->
            ( { model
                | transactionPrivate = { oldPrivate | form = { oldForm | billed_amount = FloatField (String.toFloat a) a } }
              }
            , Cmd.none
            )

        EditDescription a ->
            ( { model | transactionPrivate = { oldPrivate | form = { oldForm | description = a } } }, Cmd.none )

        ChangeCategory maybe_i a ->
            case maybe_i of
                Nothing ->
                    ( { model
                        | transactionPrivate = { oldPrivate | form = { oldForm | category = a } }
                      }
                    , Cmd.none
                    )

                Just i ->
                    case getAt i model.transactions of
                        Nothing ->
                            ( model, Cmd.none )

                        Just t ->
                            ( { model | transactions = setAt i { t | category = a } model.transactions }, Cmd.none )

        ChangeCurrency c ->
            ( { model | transactionPrivate = { oldPrivate | form = { oldForm | currency = c } } }, Cmd.none )

        ChangeFromAccount acc ->
            ( { model
                | transactionPrivate =
                    { oldPrivate
                        | form =
                            { oldForm
                                | from_account =
                                    -- Workaround an apparent bootstrap bug, where an empty string as select value misbehaves
                                    if acc == "_" then
                                        ""

                                    else
                                        acc
                            }
                    }
              }
            , Cmd.none
            )

        ChangeToAccount acc ->
            ( { model
                | transactionPrivate =
                    { oldPrivate
                        | form =
                            { oldForm
                                | to_account =
                                    if acc == "_" then
                                        ""

                                    else
                                        acc
                            }
                    }
              }
            , Cmd.none
            )

        ToDatePicker pickerType subMsg ->
            let
                ( picker, settings ) =
                    case pickerType of
                        BillDate ->
                            ( model.transactionPrivate.billDatePicker, billDateSettings )

                        TransactionDate ->
                            ( model.transactionPrivate.transactionDatePicker, transactionDateSettings )

                        AllDate ->
                            ( model.transactionPrivate.billDatePicker, billDateSettings )

                ( newDatePicker, dateEvent ) =
                    DatePicker.update settings subMsg picker

                newDate =
                    case dateEvent of
                        DatePicker.Picked changedDate ->
                            Just changedDate

                        _ ->
                            oldForm.bill_date
            in
            case pickerType of
                BillDate ->
                    ( { model
                        | transactionPrivate =
                            { oldPrivate
                                | form = { oldForm | bill_date = newDate }
                                , billDatePicker = newDatePicker
                            }
                      }
                    , Cmd.none
                    )

                TransactionDate ->
                    ( { model
                        | transactionPrivate =
                            { oldPrivate
                                | form = { oldForm | transaction_date = newDate }
                                , transactionDatePicker = newDatePicker
                            }
                      }
                    , Cmd.none
                    )

                AllDate ->
                    ( { model
                        | transactionPrivate =
                            { oldPrivate
                                | form = { oldForm | transaction_date = newDate, bill_date = newDate }
                                , transactionDatePicker = newDatePicker
                                , billDatePicker = newDatePicker
                            }
                      }
                    , Cmd.none
                    )

        GotTransactions res ->
            case res of
                Ok l ->
                    ( { model | transactions = l ++ model.transactions, message = Message.info "Added new entries" }
                    , Cmd.none
                    )

                Err (Http.BadBody e) ->
                    ( { model | message = Message.danger e }, Cmd.none )

                Err _ ->
                    ( { model | message = Message.danger "Error!" }, Cmd.none )

        FetchEditYear y ->
            ( { model
                | transactionPrivate =
                    { oldPrivate
                        | fetch = { oldFetch | year = IntField (String.toInt y) y }
                    }
              }
            , Cmd.none
            )

        FetchEditMonth m ->
            ( { model
                | transactionPrivate =
                    { oldPrivate
                        | fetch = { oldFetch | month = IntField (String.toInt m) m }
                    }
              }
            , Cmd.none
            )

        FetchEditUsername m ->
            ( { model
                | transactionPrivate =
                    { oldPrivate | fetch = { oldFetch | username = m } }
              }
            , Cmd.none
            )

        FetchEditPassword m ->
            ( { model
                | transactionPrivate =
                    { oldPrivate | fetch = { oldFetch | password = m } }
              }
            , Cmd.none
            )

        FetchChangeBackend a ->
            ( { model
                | transactionPrivate =
                    { oldPrivate | fetch = { oldFetch | selected_backend = a } }
              }
            , Cmd.none
            )

        FetchGotDate d ->
            let
                y =
                    Date.year d

                m =
                    Date.monthNumber d
            in
            ( { model
                | transactionPrivate =
                    { oldPrivate
                        | fetch =
                            { oldFetch
                                | year = IntField (Just y) (String.fromInt y)
                                , month = IntField (Just m) (String.fromInt m)
                            }
                    }
              }
            , Cmd.none
            )

        FetchGo ->
            let
                (IntField _ monthValue) =
                    model.transactionPrivate.fetch.month

                (IntField _ yearValue) =
                    model.transactionPrivate.fetch.year

                fetchCmd =
                    Http.post
                        { url = "http://localhost:8000/fetch/" ++ model.transactionPrivate.fetch.selected_backend
                        , expect = Http.expectJson GotTransactions transactionParser
                        , body =
                            Http.jsonBody
                                (E.object
                                    [ ( "user", E.string model.transactionPrivate.fetch.username )
                                    , ( "pass", E.string model.transactionPrivate.fetch.password )
                                    , ( "month", E.string monthValue )
                                    , ( "year", E.string yearValue )
                                    ]
                                )
                        }
            in
            ( { model | message = Message.info "Fetching..." }, fetchCmd )

        ClickSaveTable table ->
            let
                rows =
                    List.filter table model.transactions

                fetchCmd =
                    Http.post
                        { url = "http://localhost:8000/transactions/"
                        , expect = Http.expectWhatever (FinishSaveTable table)
                        , body =
                            Http.jsonBody (E.list transactionEncoder rows)
                        }
            in
            ( { model | message = Message.info "Saving..." }, fetchCmd )

        FinishSaveTable table res ->
            let
                inverseTable =
                    table >> not
            in
            case res of
                Ok () ->
                    ( { model | transactions = List.filter inverseTable model.transactions, message = Message.info "Saved!" }, Cmd.none )

                Err (Http.BadBody e) ->
                    ( { model | message = Message.danger e }, Cmd.none )

                Err _ ->
                    ( { model | message = Message.danger "Error!" }, Cmd.none )

        CopyCategories ->
            ( model, toClipboard (categoriesToText model.transactions) )


dateDecoder : D.Decoder Date
dateDecoder =
    let
        dateOrFail res =
            case res of
                Ok d ->
                    D.succeed d

                Err _ ->
                    D.fail "Invaid date format"
    in
    D.map Date.fromIsoString D.string |> D.andThen dateOrFail


categoriesParser : D.Decoder (List String)
categoriesParser =
    D.list (D.field "title" D.string)


transactionParser : D.Decoder (List Transaction)
transactionParser =
    D.list
        (D.succeed Transaction
            |> Dp.required "transaction_date" dateDecoder
            |> Dp.required "bill_date" dateDecoder
            |> Dp.required "transaction_amount" D.float
            |> Dp.required "billed_amount" D.float
            |> Dp.required "original_currency" (D.nullable D.string |> D.map (Maybe.withDefault "ILS"))
            |> Dp.required "description" D.string
            |> Dp.required "from_account" (D.nullable D.string |> D.map (Maybe.withDefault ""))
            |> Dp.required "to_account" (D.nullable D.string |> D.map (Maybe.withDefault ""))
            |> Dp.required "category" (D.nullable D.string |> D.map (Maybe.withDefault ""))
        )


transactionEncoder : Transaction -> E.Value
transactionEncoder t =
    let
        nullIfEmpty : String -> E.Value
        nullIfEmpty s =
            if String.isEmpty s then
                E.null

            else
                E.string s
    in
    E.object
        [ ( "transaction_date", E.string (toIsoString t.transaction_date) )
        , ( "bill_date", E.string (toIsoString t.bill_date) )
        , ( "transaction_amount", E.float t.original_amount )
        , ( "billed_amount", E.float t.billed_amount )
        , ( "original_currency", E.string t.currency )
        , ( "description", E.string t.description )
        , ( "from_account", nullIfEmpty t.from_account )
        , ( "to_account", nullIfEmpty t.to_account )
        , ( "category", nullIfEmpty t.category )
        ]


incomeFilter : Transaction -> Bool
incomeFilter t =
    String.isEmpty t.from_account && not (String.isEmpty t.to_account)


expenseFilter : Transaction -> Bool
expenseFilter t =
    not (String.isEmpty t.from_account) && String.isEmpty t.to_account


interFilter : Transaction -> Bool
interFilter t =
    not (String.isEmpty t.from_account) && not (String.isEmpty t.to_account)


rowsByCategory : List Transaction -> List ( String, Float )
rowsByCategory =
    let
        addToCategory : Transaction -> Dict String Float -> Dict String Float
        addToCategory t d =
            Dict.update t.category (\v -> Just (Maybe.withDefault 0 v + t.billed_amount)) d
    in
    List.filter expenseFilter >> List.foldl addToCategory Dict.empty >> Dict.toList


categoriesToText : List Transaction -> String
categoriesToText =
    rowsByCategory >> List.map (\( k, v ) -> k ++ ": " ++ String.fromFloat v) >> String.join " "


createTransaction : TransactionForm -> Maybe Transaction
createTransaction form =
    let
        bill_date =
            form.bill_date

        transaction_date =
            form.transaction_date

        (FloatField billed_amount _) =
            form.billed_amount

        (FloatField original_amount _) =
            form.original_amount

        oneOrMore : String -> String -> Maybe ( String, String )
        oneOrMore a b =
            if String.isEmpty a && String.isEmpty b then
                Nothing

            else
                Just ( a, b )

        newTrans bd td ba oa fa_ta =
            { transaction_date = td
            , bill_date = bd
            , billed_amount = ba
            , original_amount = oa
            , currency = form.currency
            , description = form.description
            , from_account = Tuple.first fa_ta
            , to_account = Tuple.second fa_ta
            , category = form.category
            }
    in
    Maybe.map5 newTrans bill_date transaction_date billed_amount original_amount (oneOrMore form.from_account form.to_account)


toForm : Transaction -> TransactionForm
toForm t =
    { transaction_date = Just t.transaction_date
    , bill_date = Just t.transaction_date
    , original_amount = FloatField (Just t.original_amount) (String.fromFloat t.original_amount)
    , billed_amount = FloatField (Just t.billed_amount) (String.fromFloat t.billed_amount)
    , currency = t.currency
    , description = t.description
    , from_account = t.from_account
    , to_account = t.to_account
    , category = t.category
    }



-- VIEW


selectItem : String -> Select.Item msg
selectItem o =
    Select.item [ value o ] [ text o ]


selectItemWithDefault : String -> String -> String -> Select.Item msg
selectItemWithDefault selection v txt =
    Select.item [ value v, selected (txt == selection) ] [ text txt ]


floatInput : String -> FloatField -> (String -> msg) -> Html msg
floatInput place field msg =
    case field of
        FloatField Nothing s ->
            Input.text [ Input.placeholder place, Input.value s, Input.onInput msg, Input.danger ]

        FloatField _ s ->
            Input.text [ Input.placeholder place, Input.value s, Input.onInput msg ]


intInput : String -> IntField -> (String -> msg) -> Html msg
intInput place field msg =
    case field of
        IntField Nothing s ->
            Input.text [ Input.placeholder place, Input.value s, Input.onInput msg, Input.danger ]

        IntField _ s ->
            Input.text [ Input.placeholder place, Input.value s, Input.onInput msg ]


fetchView : FetchModel -> List Account -> Html Msg
fetchView f accounts =
    let
        avail_backends =
            accounts |> List.map .backend_type |> unique
    in
    Form.formInline []
        [ Input.text [ Input.placeholder "Username", Input.value f.username, Input.onInput FetchEditUsername ]
        , Input.password [ Input.placeholder "Password", Input.value f.password, Input.onInput FetchEditPassword ]
        , intInput "Month" f.month FetchEditMonth
        , intInput "Year" f.year FetchEditYear
        , Select.select [ Select.onChange FetchChangeBackend ]
            (List.map2 (selectItemWithDefault f.selected_backend)
                ("" :: avail_backends)
                ("--" :: avail_backends)
            )
        , Button.button
            [ Button.outlinePrimary
            , Button.attrs [ Spacing.ml1, onClick FetchGo ]
            ]
            [ text "Fetch" ]
        ]


viewTable : ModelTable (TransactionModel m) -> TableType -> (Transaction -> Bool) -> Html Msg
viewTable model tt transFilter =
    let
        categories : List String
        categories =
            model.categories |> List.map .title

        tei =
            model.transactionPrivate

        entries : List ( Int, Transaction )
        entries =
            model.transactions |> List.indexedMap Tuple.pair |> List.filter (Tuple.second >> transFilter)

        listAccounts : (String -> Msg) -> String -> Html Msg
        listAccounts onInputMsg selection =
            let
                accountNames =
                    List.map .name model.accounts
            in
            Select.select [ Select.onChange onInputMsg ] (List.map2 (selectItemWithDefault selection) ("_" :: accountNames) ("---" :: accountNames))

        displayRow : Int -> Transaction -> List (Table.Cell Msg)
        displayRow i t =
            [ Table.td [] [ text (toIsoString t.transaction_date) ]
            , Table.td [] [ text (toIsoString t.bill_date) ]
            , Table.td [] [ text t.from_account ]
            , Table.td [] [ text t.to_account ]
            , Table.td [] [ text (String.fromFloat t.original_amount) ]
            , Table.td [] [ text (String.fromFloat t.billed_amount) ]
            , Table.td [] [ text t.currency ]
            , Table.td [] [ text t.description ]
            , Table.td []
                [ Select.select [ Select.onChange (ChangeCategory (Just i)) ]
                    (List.map2 (selectItemWithDefault t.category) ("" :: categories) ("--" :: categories))
                ]
            ]

        editableRow : Maybe Int -> List (Table.Cell Msg)
        editableRow maybe_i =
            [ Table.td [] [ DatePicker.view tei.form.transaction_date transactionDateSettings tei.transactionDatePicker |> Html.map (ToDatePicker TransactionDate) ]
            , Table.td [] [ DatePicker.view tei.form.bill_date billDateSettings tei.billDatePicker |> Html.map (ToDatePicker BillDate) ]
            , Table.td [] [ listAccounts ChangeFromAccount tei.form.from_account ]
            , Table.td [] [ listAccounts ChangeToAccount tei.form.to_account ]
            , Table.td [] [ floatInput "Original Amount" tei.form.original_amount EditTransAmount ]
            , Table.td [] [ floatInput "Billed Amount" tei.form.billed_amount EditBillAmount ]
            , Table.td [] [ Select.select [ Select.onChange ChangeCurrency ] (List.map selectItem currencies) ]
            , Table.td [] [ Input.text [ Input.placeholder "Description", Input.value tei.form.description, Input.onInput EditDescription ] ]
            , Table.td []
                [ Select.select [ Select.onChange (ChangeCategory Nothing) ]
                    (List.map2 (selectItemWithDefault tei.form.category) ("" :: categories) ("--" :: categories))
                ]
            ]

        toEditState : Maybe EditOp -> EditableTable.EditState
        toEditState maybe_e =
            case maybe_e of
                Nothing ->
                    EditableTable.editNothing

                Just e ->
                    case e of
                        New tt2 ->
                            if tt2 == tt then
                                EditableTable.newRow

                            else
                                EditableTable.editNothing

                        Exist i ->
                            EditableTable.editRow i

        tableEventToMessage : EditableTable.TableEvent -> Msg
        tableEventToMessage te =
            case te of
                EditableTable.StartEdit i ->
                    StartEditRow i

                EditableTable.AcceptEdit ->
                    SaveEditRow

                EditableTable.CancelEdit ->
                    CancelEditRow

                EditableTable.RemoveRow i ->
                    Remove i

                EditableTable.AddRow ->
                    Add tt

                EditableTable.SaveNewRow ->
                    SaveEditRow
    in
    Table.table
        { options = [ Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "Transaction Date" ]
                , Table.th [] [ text "Bill Date" ]
                , Table.th [] [ text "From Account" ]
                , Table.th [] [ text "To Account" ]
                , Table.th [] [ text "Transaction Amount" ]
                , Table.th [] [ text "Billed Amount" ]
                , Table.th [] [ text "Currency" ]
                , Table.th [] [ text "Description" ]
                , Table.th [] [ text "Category" ]
                ]
        , tbody =
            Table.tbody []
                (EditableTable.viewTableRows entries
                    displayRow
                    editableRow
                    tableEventToMessage
                    (toEditState tei.edit)
                )
        }


summaryTableView : List Transaction -> Html Msg
summaryTableView transactions =
    let
        compareMaybe : Maybe a -> Maybe a -> Bool
        compareMaybe a b =
            case ( a, b ) of
                ( Nothing, Nothing ) ->
                    True

                ( Just x, Just y ) ->
                    x == y

                _ ->
                    False

        compareAccounts : Transaction -> Transaction -> Bool
        compareAccounts t s =
            (t.from_account == s.from_account) && (t.to_account == s.to_account)

        sumAggregate : List Transaction -> Maybe { to : String, from : String, total : Float }
        sumAggregate t =
            case List.head t of
                Nothing ->
                    Nothing

                Just h ->
                    Just { to = h.to_account, from = h.from_account, total = List.map .billed_amount t |> List.sum }

        tuple2list : ( a, List a ) -> List a
        tuple2list ( x, y ) =
            x :: y

        rowsByAccount r =
            gatherWith compareAccounts r |> List.map tuple2list |> List.map sumAggregate |> Maybe.Extra.values

        tableRow r =
            Table.tr []
                [ Table.td [] [ text r.from ]
                , Table.td [] [ text r.to ]
                , Table.td [] [ text (r.total |> String.fromFloat) ]
                ]
    in
    Table.simpleTable
        ( Table.simpleThead
            [ Table.th [] [ text "From Account" ]
            , Table.th [] [ text "To Account" ]
            , Table.th [] [ text "Total" ]
            ]
        , Table.tbody [] (List.map tableRow (rowsByAccount transactions))
        )


categoryTableView : List Transaction -> Html msg
categoryTableView transactions =
    let
        tableRow : ( String, Float ) -> Table.Row msg
        tableRow ( c, v ) =
            Table.tr []
                [ Table.td []
                    [ if String.isEmpty c then
                        text "--"

                      else
                        text c
                    ]
                , Table.td [] [ text (String.fromFloat v) ]
                ]
    in
    Table.simpleTable
        ( Table.simpleThead
            [ Table.th [] [ text "Category" ]
            , Table.th [] [ text "Total" ]
            ]
        , Table.tbody [] (List.map tableRow (rowsByCategory transactions))
        )


view : TransactionModel m -> Html Msg
view model =
    div []
        [ CDN.stylesheet
        , fetchView model.transactionPrivate.fetch model.accounts
        , h2 []
            [ text "Expenses"
            , Button.button
                [ Button.outlinePrimary
                , Button.attrs
                    [ Spacing.m1
                    , onClick (Add ExpenseTable)
                    ]
                ]
                [ bootstrapIcon "plus" ]
            , Button.button
                [ Button.outlinePrimary
                , Button.attrs
                    [ Spacing.m1
                    , onClick (ClickSaveTable expenseFilter)
                    ]
                ]
                [ bootstrapIcon "check" ]
            ]
        , viewTable model ExpenseTable expenseFilter
        , h2 []
            [ text "Income"
            , Button.button
                [ Button.outlinePrimary
                , Button.attrs
                    [ Spacing.m1
                    , onClick (Add IncomeTable)
                    ]
                ]
                [ bootstrapIcon "plus" ]
            , Button.button
                [ Button.outlinePrimary
                , Button.attrs
                    [ Spacing.m1
                    , onClick (ClickSaveTable incomeFilter)
                    ]
                ]
                [ bootstrapIcon "check" ]
            ]
        , viewTable model IncomeTable incomeFilter
        , h2 []
            [ text "Transfers"
            , Button.button
                [ Button.outlinePrimary
                , Button.attrs
                    [ Spacing.m1
                    , onClick (Add InterTable)
                    ]
                ]
                [ bootstrapIcon "plus" ]
            , Button.button
                [ Button.outlinePrimary
                , Button.attrs
                    [ Spacing.m1
                    , onClick (ClickSaveTable interFilter)
                    ]
                ]
                [ bootstrapIcon "check" ]
            ]
        , viewTable model InterTable interFilter
        , h2 []
            [ text "Categories Summary"
            , Button.button
                [ Button.outlinePrimary
                , Button.attrs
                    [ Spacing.m1
                    , onClick CopyCategories
                    ]
                ]
                [ bootstrapIcon "clipboard" ]
            ]
        , categoryTableView model.transactions
        , div []
            [ h2 [] [ text "Account summary" ]
            , summaryTableView model.transactions
            ]
        ]
