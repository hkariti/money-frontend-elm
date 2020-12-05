port module Transactions exposing (Model, Msg, init, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
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
import List.Extra exposing (gatherEqualsBy, gatherWith, getAt, removeAt, setAt)
import Maybe.Extra exposing (cons, isJust, isNothing, join)
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


type alias Account =
    { id : Int
    , name : String
    , backend_id : String
    , backend_type : String
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
    , avail_backends : List String
    }


type alias Message =
    { level : Alert.Config Msg -> Alert.Config Msg
    , text : String
    , visibility : Alert.Visibility
    }


type TableType
    = ExpenseTable
    | IncomeTable
    | InterTable


type EditOp
    = Exist Int
    | New TableType


type alias Model =
    { transactions : List Transaction
    , edit : Maybe EditOp
    , form : TransactionForm
    , message : Message
    , billDatePicker : DatePicker.DatePicker
    , transactionDatePicker : DatePicker.DatePicker
    , fetch : FetchModel
    , accounts : List Account
    , categories : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( { transactions = []
      , edit = Nothing
      , form = emptyForm
      , billDatePicker = datePicker
      , transactionDatePicker = datePicker
      , message = noMessage
      , fetch = resetFetch
      , accounts = []
      , categories = []
      }
    , Cmd.batch
        [ getAccountsCmd
        , getCategoriesCmd
        , Cmd.map (ToDatePicker AllDate) datePickerFx
        , fetchDateCmd
        ]
    )


noMessage : Message
noMessage =
    { level = Alert.info, visibility = Alert.closed, text = "" }


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
    , avail_backends = []
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
    | GotAccounts (Result Http.Error (List Account))
    | GotCategories (Result Http.Error (List String))
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
    | AlertMsg Alert.Visibility


fetchDateCmd =
    Date.today |> Task.perform FetchGotDate


getAccountsCmd =
    Http.get
        { url = "http://localhost:8000/accounts/"
        , expect = Http.expectJson GotAccounts accountsParser
        }


getCategoriesCmd =
    Http.get
        { url = "http://localhost:8000/categories/"
        , expect = Http.expectJson GotCategories categoriesParser
        }


fetchUpdateBackends : FetchModel -> List String -> FetchModel
fetchUpdateBackends m b =
    let
        default_backend =
            List.head b |> Maybe.withDefault ""
    in
    { m | avail_backends = b, selected_backend = default_backend }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldForm =
            model.form

        oldFetch =
            model.fetch
    in
    case msg of
        Add table ->
            ( { model | form = emptyForm, edit = Just (New table) }, Cmd.none )

        Remove index ->
            ( { model | transactions = removeAt index model.transactions }, Cmd.none )

        StartEditRow n ->
            case getAt n model.transactions of
                Nothing ->
                    ( model, Cmd.none )

                Just r ->
                    ( { model | edit = Just (Exist n), form = toForm r }, Cmd.none )

        CancelEditRow ->
            ( { model | edit = Nothing, form = emptyForm }, Cmd.none )

        SaveEditRow ->
            case createTransaction model.form of
                Just t ->
                    case model.edit of
                        Nothing ->
                            ( model, Cmd.none )

                        Just e ->
                            case e of
                                Exist i ->
                                    ( { model
                                        | edit = Nothing
                                        , form = emptyForm
                                        , transactions = setAt i t model.transactions
                                      }
                                    , Cmd.none
                                    )

                                New _ ->
                                    ( { model
                                        | edit = Nothing
                                        , form = emptyForm
                                        , transactions = t :: model.transactions
                                      }
                                    , Cmd.none
                                    )

                Nothing ->
                    ( model, Cmd.none )

        EditTransAmount a ->
            ( { model | form = { oldForm | original_amount = FloatField (String.toFloat a) a } }, Cmd.none )

        EditBillAmount a ->
            ( { model | form = { oldForm | billed_amount = FloatField (String.toFloat a) a } }, Cmd.none )

        EditDescription a ->
            ( { model | form = { oldForm | description = a } }, Cmd.none )

        ChangeCategory maybe_i a ->
            case maybe_i of
                Nothing ->
                    ( { model | form = { oldForm | category = a } }, Cmd.none )

                Just i ->
                    case getAt (Debug.log "asd" i) model.transactions of
                        Nothing ->
                            ( model, Cmd.none )

                        Just t ->
                            ( { model | transactions = setAt i { t | category = a } model.transactions }, Cmd.none )

        ChangeCurrency c ->
            ( { model | form = { oldForm | currency = c } }, Cmd.none )

        ChangeFromAccount acc ->
            ( { model
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
            , Cmd.none
            )

        ChangeToAccount acc ->
            ( { model
                | form =
                    { oldForm
                        | to_account =
                            if acc == "_" then
                                ""

                            else
                                acc
                    }
              }
            , Cmd.none
            )

        ToDatePicker pickerType subMsg ->
            let
                ( picker, settings ) =
                    case pickerType of
                        BillDate ->
                            ( model.billDatePicker, billDateSettings )

                        TransactionDate ->
                            ( model.transactionDatePicker, transactionDateSettings )

                        AllDate ->
                            ( model.billDatePicker, billDateSettings )

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
                        | form = { oldForm | bill_date = newDate }
                        , billDatePicker = newDatePicker
                      }
                    , Cmd.none
                    )

                TransactionDate ->
                    ( { model
                        | form = { oldForm | transaction_date = newDate }
                        , transactionDatePicker = newDatePicker
                      }
                    , Cmd.none
                    )

                AllDate ->
                    ( { model
                        | form = { oldForm | transaction_date = newDate, bill_date = newDate }
                        , transactionDatePicker = newDatePicker
                        , billDatePicker = newDatePicker
                      }
                    , Cmd.none
                    )

        GotTransactions res ->
            case res of
                Ok l ->
                    ( { model | transactions = l ++ model.transactions, message = Message Alert.info "Added new entries" Alert.shown }
                    , Cmd.none
                    )

                Err (Http.BadBody e) ->
                    ( { model | message = Message Alert.danger e Alert.shown }, Cmd.none )

                Err _ ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown }, Cmd.none )

        FetchEditYear y ->
            ( { model | fetch = { oldFetch | year = IntField (String.toInt y) y } }, Cmd.none )

        FetchEditMonth m ->
            ( { model | fetch = { oldFetch | month = IntField (String.toInt m) m } }, Cmd.none )

        FetchEditUsername m ->
            ( { model | fetch = { oldFetch | username = m } }, Cmd.none )

        FetchEditPassword m ->
            ( { model | fetch = { oldFetch | password = m } }, Cmd.none )

        FetchChangeBackend a ->
            ( { model | fetch = { oldFetch | selected_backend = a } }, Cmd.none )

        GotAccounts res ->
            case res of
                Ok a ->
                    let
                        backends =
                            List.map .backend_type a |> List.Extra.unique
                    in
                    ( { model | accounts = a, fetch = fetchUpdateBackends oldFetch backends }, Cmd.none )

                Err (Http.BadBody e) ->
                    ( { model | message = Message Alert.danger e Alert.shown }, Cmd.none )

                Err _ ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown }, Cmd.none )

        GotCategories res ->
            case res of
                Ok a ->
                    ( { model | categories = a }, Cmd.none )

                Err (Http.BadBody e) ->
                    ( { model | message = Message Alert.danger e Alert.shown }, Cmd.none )

                Err _ ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown }, Cmd.none )

        FetchGotDate d ->
            let
                y =
                    Date.year d

                m =
                    Date.monthNumber d
            in
            ( { model
                | fetch =
                    { oldFetch
                        | year = IntField (Just y) (String.fromInt y)
                        , month = IntField (Just m) (String.fromInt m)
                    }
              }
            , Cmd.none
            )

        FetchGo ->
            let
                (IntField _ monthValue) =
                    model.fetch.month

                (IntField _ yearValue) =
                    model.fetch.year

                fetchCmd =
                    Http.post
                        { url = "http://localhost:8000/fetch/" ++ model.fetch.selected_backend
                        , expect = Http.expectJson GotTransactions transactionParser
                        , body =
                            Http.jsonBody
                                (E.object
                                    [ ( "user", E.string model.fetch.username )
                                    , ( "pass", E.string model.fetch.password )
                                    , ( "month", E.string monthValue )
                                    , ( "year", E.string yearValue )
                                    ]
                                )
                        }
            in
            ( { model | message = Message Alert.info "Fetching..." Alert.shown }, fetchCmd )

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
            ( { model | message = Message Alert.info "Saving..." Alert.shown }, fetchCmd )

        FinishSaveTable table res ->
            let
                inverseTable =
                    table >> not
            in
            case res of
                Ok () ->
                    ( { model | transactions = List.filter inverseTable model.transactions, message = Message Alert.info "Saved!" Alert.shown }, Cmd.none )

                Err (Http.BadBody e) ->
                    ( { model | message = Message Alert.danger e Alert.shown }, Cmd.none )

                Err _ ->
                    ( { model | message = Message Alert.danger "Error!" Alert.shown }, Cmd.none )

        CopyCategories ->
            ( model, toClipboard (categoriesToText model.transactions) )

        AlertMsg m ->
            let
                oldMessage =
                    model.message
            in
            ( { model | message = { oldMessage | visibility = m } }, Cmd.none )


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


accountsParser : D.Decoder (List Account)
accountsParser =
    D.list
        (D.map4 Account
            (D.field "id" D.int)
            (D.field "name" D.string)
            (D.field "backend_id" D.string)
            (D.field "backend_type" D.string)
        )


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



-- VIEW




viewTable : Model -> TableType -> (Transaction -> Bool) -> Html Msg
viewTable model tt transFilter =
    let
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
                    (List.map2 (selectItemWithDefault t.category) ("" :: model.categories) ("--" :: model.categories))
                ]
            ]

        editableRow : Maybe Int -> List (Table.Cell Msg)
        editableRow maybe_i =
            [ Table.td [] [ DatePicker.view model.form.transaction_date transactionDateSettings model.transactionDatePicker |> Html.map (ToDatePicker TransactionDate) ]
            , Table.td [] [ DatePicker.view model.form.bill_date billDateSettings model.billDatePicker |> Html.map (ToDatePicker BillDate) ]
            , Table.td [] [ listAccounts ChangeFromAccount model.form.from_account ]
            , Table.td [] [ listAccounts ChangeToAccount model.form.to_account ]
            , Table.td [] [ floatInput "Original Amount" model.form.original_amount EditTransAmount ]
            , Table.td [] [ floatInput "Billed Amount" model.form.billed_amount EditBillAmount ]
            , Table.td [] [ Select.select [ Select.onChange ChangeCurrency ] (List.map selectItem currencies) ]
            , Table.td [] [ Input.text [ Input.placeholder "Description", Input.value model.form.description, Input.onInput EditDescription ] ]
            , Table.td []
                [ Select.select [ Select.onChange (ChangeCategory Nothing) ]
                    (List.map2 (selectItemWithDefault model.form.category) ("" :: model.categories) ("--" :: model.categories))
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
                    (toEditState model.edit)
                )
        }


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet
        , Alert.config
            |> Alert.dismissable AlertMsg
            |> model.message.level
            |> Alert.children [ text model.message.text ]
            |> Alert.view model.message.visibility
        , fetchView model.fetch
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
        , categoryTableView model
        , div []
            [ h2 [] [ text "Account summary" ]
            , summaryTableView model
            ]
        ]


summaryTableView : Model -> Html Msg
summaryTableView model =
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
        , Table.tbody [] (List.map tableRow (rowsByAccount model.transactions))
        )


categoryTableView : Model -> Html Msg
categoryTableView model =
    let
        tableRow : ( String, Float ) -> Table.Row Msg
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
        , Table.tbody [] (List.map tableRow (rowsByCategory model.transactions))
        )


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


fetchView : FetchModel -> Html Msg
fetchView f =
    Form.formInline []
        [ Input.text [ Input.placeholder "Username", Input.value f.username, Input.onInput FetchEditUsername ]
        , Input.password [ Input.placeholder "Password", Input.value f.password, Input.onInput FetchEditPassword ]
        , intInput "Month" f.month FetchEditMonth
        , intInput "Year" f.year FetchEditYear
        , Select.select [ Select.onChange FetchChangeBackend ]
            (List.map2 (selectItemWithDefault f.selected_backend)
                f.avail_backends
                f.avail_backends
            )
        , Button.button
            [ Button.outlinePrimary
            , Button.attrs [ Spacing.ml1, onClick FetchGo ]
            ]
            [ text "Fetch" ]
        ]
