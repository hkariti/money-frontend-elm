port module Transactions exposing (Model, Msg, init, update, view)

import Date exposing (Date, fromIsoString, toIsoString)
import DatePicker exposing (defaultSettings)
import Debug
import Dict exposing (Dict)
import Html exposing (Html, button, div, h2, input, option, select, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Decode.Pipeline as Dp
import Json.Encode as E
import List.Extra exposing (gatherEqualsBy, gatherWith, getAt, removeAt, setAt)
import Maybe.Extra exposing (isJust, isNothing, join)
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


type alias Model =
    { transactions : List Transaction
    , edit : Maybe Int
    , form : TransactionForm
    , billDatePicker : DatePicker.DatePicker
    , transactionDatePicker : DatePicker.DatePicker
    , message : String
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
      , message = ""
      , fetch = resetFetch
      , accounts = []
      , categories = []
      }
    , Cmd.batch
        [ getAccountsCmd
        , getCategoriesCmd
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
    = Add
    | Remove Int
    | StartEditRow Int
    | SaveEditRow
    | CancelEditRow
    | EditTransAmount String
    | EditBillAmount String
    | EditDescription String
    | ChangeCategory Int String
    | ChangeCurrency Currency
    | ChangeFromAccount String
    | ChangeToAccount String
    | ToDatePicker DatePickerType DatePicker.Msg
    | GotTransactions (Result Http.Error (List Transaction))
    | GotAccounts (Result Http.Error (List Account))
    | GotCategories (Result Http.Error (List String))
    | FetchOpen
    | FetchClose
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
        Add ->
            case createTransaction model.form of
                Just t ->
                    ( { model | form = emptyForm, transactions = t :: model.transactions }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Remove index ->
            ( { model | transactions = removeAt index model.transactions }, Cmd.none )

        StartEditRow n ->
            case getAt n model.transactions of
                Nothing ->
                    ( model, Cmd.none )

                Just r ->
                    ( { model | edit = Just n, form = toForm r }, Cmd.none )

        CancelEditRow ->
            ( { model | edit = Nothing, form = emptyForm }, Cmd.none )

        SaveEditRow ->
            case createTransaction model.form of
                Just t ->
                    case model.edit of
                        Just i ->
                            ( { model
                                | edit = Nothing
                                , form = emptyForm
                                , transactions = setAt i t model.transactions
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTransAmount a ->
            ( { model | form = { oldForm | original_amount = FloatField (String.toFloat a) a } }, Cmd.none )

        EditBillAmount a ->
            ( { model | form = { oldForm | billed_amount = FloatField (String.toFloat a) a } }, Cmd.none )

        EditDescription a ->
            ( { model | form = { oldForm | description = a } }, Cmd.none )

        ChangeCategory i a ->
            if i < 0 then
                ( { model | form = { oldForm | category = a } }, Cmd.none )

            else
                let
                    oldTrans =
                        getAt i model.transactions
                in
                case oldTrans of
                    Nothing ->
                        ( model, Cmd.none )

                    Just t ->
                        ( { model | transactions = setAt i { t | category = a } model.transactions }, Cmd.none )

        ChangeCurrency c ->
            ( { model | form = { oldForm | currency = c } }, Cmd.none )

        ChangeFromAccount acc ->
            ( { model | form = { oldForm | from_account = acc } }, Cmd.none )

        ChangeToAccount acc ->
            ( { model | form = { oldForm | to_account = acc } }, Cmd.none )

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
                    ( { model | transactions = l ++ model.transactions, message = "Added new entries" }
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

        GotCategories res ->
            case res of
                Ok a ->
                    ( { model | categories = a }, Cmd.none )

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
            ( { model | message = "Saving..." }, fetchCmd )

        FinishSaveTable table res ->
            let
                inverseTable =
                    table >> not
            in
            case res of
                Ok () ->
                    ( { model | transactions = List.filter inverseTable model.transactions, message = "Saved!" }, Cmd.none )

                Err (Http.BadBody e) ->
                    ( { model | message = e }, Cmd.none )

                Err _ ->
                    ( { model | message = "Error!" }, Cmd.none )

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
                    option [ value a.name, selected (a.name == selection) ] [ text a.name ]
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
             , input [ placeholder "Description", value model.form.description, onInput EditDescription ] []
             , select [ onInput (ChangeCategory -1) ]
                (option [ value "" ] [ text "--" ] :: List.map selectOption model.categories)
             ]
                ++ globalActions model.edit model.fetch
            )
        , div [ class "messagebox" ] [ text model.message ]
        , div []
            [ h2 []
                [ text "Income"
                , button [ style "font-size" "0.5em", onClick (ClickSaveTable incomeFilter) ] [ text "Save & clear" ]
                ]
            , tableView model incomeFilter
            ]
        , div []
            [ h2 []
                [ text "Expenses"
                , button [ style "font-size" "0.5em", onClick (ClickSaveTable expenseFilter) ] [ text "Save & clear" ]
                ]
            , tableView model expenseFilter
            ]
        , div []
            [ h2 []
                [ text "Inter-account transfers"
                , button [ style "font-size" "0.5em", onClick (ClickSaveTable interFilter) ] [ text "Save & clear" ]
                ]
            , tableView model interFilter
            ]
        , div []
            [ h2 [] [ text "Account summary" ]
            , summaryTableView model
            ]
        , div []
            [ h2 []
                [ text "Category summary"
                , button [ style "font-size" "0.5em", onClick CopyCategories ] [ text "Copy" ]
                ]
            , categoryTableView model
            ]
        ]


tableView : Model -> (Transaction -> Bool) -> Html Msg
tableView model rowFilter =
    let
        entries : List ( Int, Transaction )
        entries =
            model.transactions |> List.indexedMap Tuple.pair |> List.filter (Tuple.second >> rowFilter)

        toTableRow : ( Int, Transaction ) -> Html Msg
        toTableRow ( i, t ) =
            let
                rowStyle =
                    (case Maybe.map ((==) i) model.edit of
                        Just True ->
                            [ style "border-style" "solid" ]

                        _ ->
                            []
                    )
                        ++ (if t.from_account == t.to_account then
                                [ style "background" "red" ]

                            else if String.isEmpty t.from_account then
                                [ style "background" "lightblue" ]

                            else if String.isEmpty t.to_account then
                                [ style "background" "pink" ]

                            else
                                [ style "background" "white" ]
                           )
            in
            tr rowStyle
                [ td [] [ text (toIsoString t.transaction_date) ]
                , td [] [ text (toIsoString t.bill_date) ]
                , td [] [ text t.from_account ]
                , td [] [ text t.to_account ]
                , td [] [ text (String.fromFloat t.original_amount) ]
                , td [] [ text (String.fromFloat t.billed_amount) ]
                , td [] [ text t.currency ]
                , td [] [ text t.description ]
                , td []
                    [ select [ onInput (ChangeCategory i) ]
                        (List.map2 (selectOptionWithDefault t.category) ("" :: model.categories) ("--" :: model.categories))
                    ]
                , td []
                    [ button [ onClick (Remove i) ] [ text "Remove" ]
                    , button [ onClick (StartEditRow i) ] [ text "Edit" ]
                    ]
                ]
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
            , th [] [ text "Category" ]
            , th [] [ text "Actions" ]
            ]
         ]
            ++ List.map toTableRow entries
        )


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
            tr []
                [ td [] [ text r.from ]
                , td [] [ text r.to ]
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
            ++ List.map tableRow (rowsByAccount model.transactions)
        )


categoryTableView : Model -> Html Msg
categoryTableView model =
    let
        tableRow : ( String, Float ) -> Html Msg
        tableRow ( c, v ) =
            tr []
                [ td []
                    [ if String.isEmpty c then
                        text "--"

                      else
                        text c
                    ]
                , tr [] [ text (String.fromFloat v) ]
                ]
    in
    table [ style "border-collapse" "collapse", style "margin" "5px" ]
        ([ tr []
            [ th [] [ text "Category" ]
            , th [] [ text "Total" ]
            ]
         ]
            ++ List.map tableRow (rowsByCategory model.transactions)
        )


selectOption : String -> Html msg
selectOption o =
    option [] [ text o ]


selectOptionWithDefault : String -> String -> String -> Html msg
selectOptionWithDefault selection v txt =
    option [ value v, selected (txt == selection) ] [ text txt ]


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


globalActions : Maybe x -> FetchModel -> List (Html Msg)
globalActions e f =
    case e of
        Just _ ->
            [ button [ onClick SaveEditRow ] [ text "Save" ]
            , button [ onClick CancelEditRow ] [ text "Cancel" ]
            ]

        Nothing ->
            [ button [ onClick Add ] [ text "Add" ] ]
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
                , select [ onInput FetchChangeBackend ]
                    (List.map2 (selectOptionWithDefault f.selected_backend)
                        f.avail_backends
                        f.avail_backends
                    )
                , button [ onClick FetchGo ] [ text "Go" ]
                ]
            ]

        False ->
            [ button [ onClick FetchOpen ] [ text "Fetch" ] ]
