module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Date exposing (Date, fromIsoString, toIsoString)
import DatePicker exposing (defaultSettings)
import Debug
import Html exposing (Html, button, div, input, option, select, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import List.Extra exposing (gatherWith, getAt, removeAt, setAt)
import Maybe.Extra exposing (isJust, isNothing, join)
import Task



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }



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
    , from_account : Maybe Int
    , to_account : Maybe Int
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


type alias Model =
    { incomeTable : List Transaction
    , outcomeTable : List Transaction
    , interTable : List Transaction
    , edit : Maybe ( TableSelect, Int )
    , form : TransactionForm
    , billDatePicker : DatePicker.DatePicker
    , transactionDatePicker : DatePicker.DatePicker
    , message : String
    , fetch : FetchModel
    , accounts : List Account
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( { incomeTable = []
      , outcomeTable = []
      , interTable = []
      , edit = Nothing
      , form = emptyForm
      , billDatePicker = datePicker
      , transactionDatePicker = datePicker
      , message = ""
      , fetch = resetFetch
      , accounts = []
      }
    , Cmd.batch
        [ getAccountsCmd
        , Cmd.map (ToDatePicker AllDate) datePickerFx
        ]
    )


billDateSettings : DatePicker.Settings
billDateSettings =
    { defaultSettings | placeholder = "Bill Date" }


transactionDateSettings : DatePicker.Settings
transactionDateSettings =
    { defaultSettings | placeholder = "Transaction Date" }


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


type TableSelect
    = IncomeTable
    | OutcomeTable
    | InterTable


type Msg
    = Add TableSelect
    | Remove TableSelect Int
    | StartEditRow TableSelect Int
    | SaveEditRow
    | CancelEditRow
    | EditTransAmount String
    | EditBillAmount String
    | ChangeCurrency Currency
    | ChangeFromAccount String
    | ChangeToAccount String
    | ToDatePicker DatePickerType DatePicker.Msg
    | GotTransactions (Result Http.Error (List Transaction))
    | GotAccounts (Result Http.Error (List Account))
    | FetchOpen
    | FetchClose
    | FetchEditYear String
    | FetchEditMonth String
    | FetchEditUsername String
    | FetchEditPassword String
    | FetchChangeBackend String
    | FetchGotDate Date
    | FetchGo


getTable : Model -> TableSelect -> List Transaction
getTable model tableSel =
    case tableSel of
        IncomeTable ->
            model.incomeTable

        OutcomeTable ->
            model.outcomeTable

        InterTable ->
            model.interTable


updateTable : Model -> TableSelect -> (List Transaction -> List Transaction) -> Model
updateTable model tableSel updateFunc =
    case tableSel of
        IncomeTable ->
            { model | incomeTable = updateFunc model.incomeTable }

        OutcomeTable ->
            { model | outcomeTable = updateFunc model.outcomeTable }

        InterTable ->
            { model | interTable = updateFunc model.interTable }


getAccountsCmd =
    Http.get
        { url = "http://localhost:8000/accounts/"
        , expect = Http.expectJson GotAccounts accountsParser
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
        Add tableSel ->
            case createTransaction model.form of
                Just t ->
                    let
                        updateTableFunc oldTable =
                            oldTable ++ [ t ]

                        updatedModel =
                            updateTable model tableSel updateTableFunc
                    in
                    ( { updatedModel | form = emptyForm }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Remove tableSel index ->
            ( updateTable model tableSel (removeAt index), Cmd.none )

        StartEditRow tableSel n ->
            let
                table =
                    getTable model tableSel
            in
            case getAt n table of
                Nothing ->
                    ( model, Cmd.none )

                Just r ->
                    ( { model | edit = Just ( tableSel, n ), form = toForm r }, Cmd.none )

        CancelEditRow ->
            ( { model | edit = Nothing, form = emptyForm }, Cmd.none )

        SaveEditRow ->
            case createTransaction model.form of
                Just t ->
                    case model.edit of
                        Just ( tableSel, i ) ->
                            let
                                updatedModel =
                                    updateTable model tableSel (setAt i t)
                            in
                            ( { updatedModel | edit = Nothing, form = emptyForm }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTransAmount a ->
            ( { model | form = { oldForm | original_amount = FloatField (String.toFloat a) a } }, Cmd.none )

        EditBillAmount a ->
            ( { model | form = { oldForm | billed_amount = FloatField (String.toFloat a) a } }, Cmd.none )

        ChangeCurrency c ->
            ( { model | form = { oldForm | currency = c } }, Cmd.none )

        ChangeFromAccount id ->
            ( { model | form = { oldForm | from_account = id } }, Cmd.none )

        ChangeToAccount id ->
            ( { model | form = { oldForm | to_account = id } }, Cmd.none )

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

        --Fetch ->
        --    ( model
        --    , Http.get
        --        { url = "http://localhost:8000/transactions/"
        --        , expect = Http.expectJson GotTransactions transactionParser
        --        }
        --    )
        GotTransactions res ->
            case res of
                Ok l ->
                    let
                        isIncome : Transaction -> Bool
                        isIncome t =
                            isNothing t.from_account && isJust t.to_account

                        isOutcome : Transaction -> Bool
                        isOutcome t =
                            isNothing t.to_account && isJust t.from_account

                        filter1 =
                            List.partition isIncome l

                        filter2 =
                            List.partition isOutcome (Tuple.second filter1)

                        incomeTrans =
                            Tuple.first filter1

                        outcomeTrans =
                            Tuple.first filter2

                        interTrans =
                            Tuple.second filter2
                    in
                    ( { model
                        | incomeTable = model.incomeTable ++ incomeTrans
                        , outcomeTable = model.outcomeTable ++ outcomeTrans
                        , interTable = model.interTable ++ interTrans
                        , message = "Added new entries"
                      }
                    , Cmd.none
                    )

                Err (Http.BadBody e) ->
                    ( { model | message = e }, Cmd.none )

                Err _ ->
                    ( { model | message = "Error!" }, Cmd.none )

        FetchClose ->
            ( { model | fetch = { oldFetch | open = False } }, Cmd.none )

        FetchOpen ->
            let
                fetchDateCmd =
                    Date.today |> Task.perform FetchGotDate
            in
            ( { model | fetch = { oldFetch | open = True } }, fetchDateCmd )

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
                    ( { model | message = e }, Cmd.none )

                Err _ ->
                    ( { model | message = "Error!" }, Cmd.none )

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
            ( { model | message = "Fetching..." }, fetchCmd )


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
        (D.map8 Transaction
            (D.field "transaction_date" dateDecoder)
            (D.field "bill_date" dateDecoder)
            (D.field "transaction_amount" D.float)
            (D.field "billed_amount" D.float)
            (D.field "original_currency" (D.nullable D.string |> D.map (Maybe.withDefault "ILS")))
            (D.field "description" D.string)
            (D.field "from_account" (D.nullable D.int))
            (D.field "to_account" (D.nullable D.int))
        )



-- VIEW


view : Model -> Html Msg
view model =
    let
        listAccounts : (String -> Msg) -> String -> Html Msg
        listAccounts onInputMsg selection =
            let
                emptyAccount : Html Msg
                emptyAccount =
                    option [ value "", selected (selection == "") ] [ text "--- Outside ---" ]

                accountOption : Account -> Html Msg
                accountOption a =
                    option [ value (String.fromInt a.id), selected (String.fromInt a.id == selection) ] [ text a.name ]
            in
            select [ onInput onInputMsg ] (emptyAccount :: List.map accountOption model.accounts)
    in
    div []
        [ div []
            ([ listAccounts ChangeFromAccount model.form.from_account
             , listAccounts ChangeToAccount model.form.to_account
             , DatePicker.view model.form.transaction_date transactionDateSettings model.transactionDatePicker |> Html.map (ToDatePicker TransactionDate)
             , DatePicker.view model.form.bill_date billDateSettings model.billDatePicker |> Html.map (ToDatePicker BillDate)
             , floatInput "Original Amount" model.form.original_amount EditTransAmount
             , floatInput "Billed Amount" model.form.billed_amount EditBillAmount
             , select [ onInput ChangeCurrency ] (List.map selectOption currencies)
             ]
                ++ globalActions model.edit model.fetch
            )
        , div [ class "messagebox" ] [ text model.message ]
        , tableView model IncomeTable
        , tableView model OutcomeTable
        , tableView model InterTable
        , summaryTableView model
        ]


tableView : Model -> TableSelect -> Html Msg
tableView model tableSel =
    let
        entries =
            getTable model tableSel

        currentEdit =
            model.edit
                |> Maybe.andThen
                    (\( t, i ) ->
                        if t == tableSel then
                            Just i

                        else
                            Nothing
                    )
    in
    table [ style "border-collapse" "collapse", style "margin" "5px" ]
        ([ tr []
            [ th [] [ text "Transaction Date" ]
            , th [] [ text "Bill Date" ]
            , th [] [ text "From Account" ]
            , th [] [ text "To Account" ]
            , th [] [ text "Original Amount" ]
            , th [] [ text "Billed Amount" ]
            , th [] [ text "Currency" ]
            , th [] [ text "Description" ]
            , th [] [ text "Actions" ]
            ]
         ]
            ++ List.indexedMap (toTableRow model.accounts tableSel currentEdit) entries
        )


toTableRow : List Account -> TableSelect -> Maybe Int -> Int -> Transaction -> Html Msg
toTableRow accounts tableSel edit i t =
    let
        rowStyle =
            (case Maybe.map ((==) i) edit of
                Just True ->
                    [ style "border-style" "solid" ]

                _ ->
                    []
            )
                ++ (case ( t.from_account, t.to_account ) of
                        ( Nothing, Just _ ) ->
                            [ style "background" "lightblue" ]

                        ( Just _, Nothing ) ->
                            [ style "background" "pink" ]

                        ( Nothing, Nothing ) ->
                            [ style "background" "red" ]

                        ( Just _, Just _ ) ->
                            [ style "background" "white" ]
                   )

        accountName id =
            List.filter (.id >> (==) id) accounts |> List.head |> Maybe.map .name |> Maybe.withDefault "Invalid account"
    in
    tr rowStyle
        [ td [] [ text (toIsoString t.transaction_date) ]
        , td [] [ text (toIsoString t.bill_date) ]
        , td [] [ text (t.from_account |> Maybe.map accountName |> Maybe.withDefault "") ]
        , td [] [ text (t.to_account |> Maybe.map accountName |> Maybe.withDefault "") ]
        , td [] [ text (String.fromFloat t.original_amount) ]
        , td [] [ text (String.fromFloat t.billed_amount) ]
        , td [] [ text t.currency ]
        , td [] [ text t.description ]
        , td []
            [ button [ onClick (Remove tableSel i) ] [ text "Remove" ]
            , button [ onClick (StartEditRow tableSel i) ] [ text "Edit" ]
            ]
        ]


summaryTableView : Model -> Html Msg
summaryTableView model =
    let
        all_rows =
            model.incomeTable ++ model.outcomeTable ++ model.interTable

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
            compareMaybe t.from_account s.from_account && compareMaybe t.to_account s.to_account

        sumAggregate : List Transaction -> Maybe { to : Maybe Int, from : Maybe Int, total : Float }
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

        accountName id =
            List.filter (.id >> (==) id) model.accounts |> List.head |> Maybe.map .name |> Maybe.withDefault "Invalid account"

        tableRow r =
            tr []
                [ td [] [ text (r.from |> Maybe.map accountName |> Maybe.withDefault "") ]
                , td [] [ text (r.to |> Maybe.map accountName |> Maybe.withDefault "") ]
                , td [] [ text (r.total |> String.fromFloat) ]
                ]
    in
    table [ style "border-collapse" "collapse", style "margin" "5px" ]
        ([ tr []
            [ th [] [ text "From Account" ]
            , th [] [ text "To Account" ]
            , th [] [ text "Total" ]
            ]
         ]
            ++ List.map tableRow (rowsByAccount all_rows)
        )


selectOption : String -> Html msg
selectOption o =
    option [] [ text o ]


floatInput : String -> FloatField -> (String -> msg) -> Html msg
floatInput place field msg =
    case field of
        FloatField Nothing s ->
            input [ placeholder place, value s, onInput msg, style "border-color" "red" ] []

        FloatField _ s ->
            input [ placeholder place, value s, onInput msg ] []


intInput : String -> IntField -> (String -> msg) -> Html msg
intInput place field msg =
    case field of
        IntField Nothing s ->
            input [ placeholder place, value s, onInput msg, style "border-color" "red" ] []

        IntField _ s ->
            input [ placeholder place, value s, onInput msg ] []


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

        from_account =
            String.toInt form.from_account

        to_account =
            String.toInt form.to_account

        oneOrMore : Maybe a -> Maybe b -> Maybe ( Maybe a, Maybe b )
        oneOrMore a b =
            case ( a, b ) of
                ( Nothing, Nothing ) ->
                    Nothing

                _ ->
                    Just ( a, b )

        newTrans bd td ba oa fa_ta =
            { transaction_date = td
            , bill_date = bd
            , billed_amount = ba
            , original_amount = oa
            , currency = form.currency
            , description = ""
            , from_account = Tuple.first fa_ta
            , to_account = Tuple.second fa_ta
            }
    in
    Maybe.map5 newTrans bill_date transaction_date billed_amount original_amount (oneOrMore from_account to_account)


toForm : Transaction -> TransactionForm
toForm t =
    { transaction_date = Just t.transaction_date
    , bill_date = Just t.transaction_date
    , original_amount = FloatField (Just t.original_amount) (String.fromFloat t.original_amount)
    , billed_amount = FloatField (Just t.billed_amount) (String.fromFloat t.billed_amount)
    , currency = t.currency
    , description = t.description
    , from_account = t.from_account |> Maybe.map String.fromInt |> Maybe.withDefault ""
    , to_account = t.to_account |> Maybe.map String.fromInt |> Maybe.withDefault ""
    }


globalActions : Maybe x -> FetchModel -> List (Html Msg)
globalActions e f =
    case e of
        Just _ ->
            [ button [ onClick SaveEditRow ] [ text "Save" ]
            , button [ onClick CancelEditRow ] [ text "Cancel" ]
            ]

        Nothing ->
            [ button [ onClick (Add OutcomeTable) ] [ text "Add" ] ]
                ++ fetchView f


fetchView : FetchModel -> List (Html Msg)
fetchView f =
    case f.open of
        True ->
            [ button [ onClick FetchClose ] [ text "Fetch" ]
            , div []
                [ input [ placeholder "Username", value f.username, onInput FetchEditUsername ] []
                , input [ placeholder "Password", value f.password, onInput FetchEditPassword ] []
                , intInput "Month" f.month FetchEditMonth
                , intInput "Year" f.year FetchEditYear
                , select [ onInput FetchChangeBackend ] (List.map selectOption f.avail_backends)
                , button [ onClick FetchGo ] [ text "Go" ]
                ]
            ]

        False ->
            [ button [ onClick FetchOpen ] [ text "Fetch" ] ]
