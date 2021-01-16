module Summary exposing (Msg, SummaryPrivate, init, update, view)

import Account
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Categories
import Date exposing (Date)
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
import Task
import Transactions exposing (Transaction, categoryTableView, transactionParser)



-- INIT


type Operation
    = UpdateOp
    | DeleteOp
    | FetchOp


type IntField
    = IntField (Maybe Int) String


type alias SummaryPrivate =
    { edit : Maybe Int
    , currentOp : Maybe Operation
    , transactions : List Transaction
    , month : Maybe Date.Month
    , year : IntField
    }


type alias SummaryModel m =
    { m
        | summaryPrivate : SummaryPrivate
        , message : Message.Model
    }


init : () -> ( SummaryPrivate, Cmd Msg )
init _ =
    ( SummaryPrivate Nothing Nothing [] Nothing (IntField Nothing ""), Date.today |> Task.perform ReceiveDate )



-- UPDATE


type Msg
    = CancelEdit
    | GotTransactions (Result Http.Error (List Transaction))
    | ReceiveDate Date
    | EditYear String
    | ChangeMonth String
    | Submit


getTransactionByDateCmd : Int -> Maybe Date.Month -> Cmd Msg
getTransactionByDateCmd year maybe_month =
    let
        month_url =
            case maybe_month of
                Nothing ->
                    []

                Just month ->
                    [ month |> Date.monthToNumber |> String.fromInt, "/" ]
    in
    Http.get
        { url = String.concat ([ "http://localhost:8000/transactions/by_date/", String.fromInt year, "/" ] ++ month_url)
        , expect = Http.expectJson GotTransactions transactionParser
        }


update : Msg -> SummaryModel m -> ( SummaryModel m, Cmd Msg )
update msg model =
    let
        oldPrivate =
            model.summaryPrivate
    in
    case msg of
        CancelEdit ->
            ( model, Cmd.none )

        GotTransactions res ->
            case res of
                Ok t ->
                    ( { model | summaryPrivate = { oldPrivate | currentOp = Nothing, transactions = t } }, Cmd.none )

                Err (Http.BadBody e) ->
                    ( { model | message = Message.danger e, summaryPrivate = { oldPrivate | currentOp = Nothing } }, Cmd.none )

                Err _ ->
                    ( { model | message = Message.danger "Error!", summaryPrivate = { oldPrivate | currentOp = Nothing } }, Cmd.none )

        ReceiveDate d ->
            let
                y =
                    Date.year d
            in
            ( { model | summaryPrivate = { oldPrivate | year = IntField (Just y) (String.fromInt y), month = Nothing } }, Cmd.none )

        EditYear v ->
            ( { model | summaryPrivate = { oldPrivate | year = IntField (String.toInt v) v } }, Cmd.none )

        ChangeMonth m ->
            ( { model | summaryPrivate = { oldPrivate | month = String.toInt m |> Maybe.map Date.numberToMonth } }, Cmd.none )

        Submit ->
            let
                (IntField y _) =
                    model.summaryPrivate.year
            in
            case y of
                Nothing ->
                    ( model, Cmd.none )

                Just year ->
                    ( { model | summaryPrivate = { oldPrivate | currentOp = Just FetchOp } }, getTransactionByDateCmd year model.summaryPrivate.month )



-- VIEW


intInput : String -> IntField -> (String -> msg) -> Html msg
intInput place field msg =
    case field of
        IntField Nothing s ->
            Input.text [ Input.placeholder place, Input.value s, Input.onInput msg, Input.danger ]

        IntField _ s ->
            Input.text [ Input.placeholder place, Input.value s, Input.onInput msg ]


view : SummaryModel m -> Html Msg
view model =
    let
        totalIncome =
            model.summaryPrivate.transactions
                |> List.filter (\t -> String.isEmpty t.from_account && not (String.isEmpty t.to_account))
                |> List.map .billed_amount
                |> List.sum

        totalExpenses =
            model.summaryPrivate.transactions
                |> List.filter (\t -> String.isEmpty t.to_account && not (String.isEmpty t.from_account))
                |> List.map .billed_amount
                |> List.sum

        spinnerIfOp : Operation -> Html msg -> Html msg
        spinnerIfOp o e =
            MExtra.filter ((==) o) model.summaryPrivate.currentOp
                |> Maybe.map
                    (\_ ->
                        Button.button [ Button.outlinePrimary, Button.attrs [] ]
                            [ Spinner.spinner [ Spinner.small, Spinner.attrs [ Spacing.m1 ] ] []
                            ]
                    )
                |> Maybe.withDefault e
    in
    div []
        [ Form.formInline []
            [ intInput "Year" model.summaryPrivate.year EditYear
            , Select.select [ Select.onChange ChangeMonth ]
                (List.map (\v -> Select.item [ value v ] [ text v ])
                    ("--" :: List.map String.fromInt (List.range 1 12))
                )
            , spinnerIfOp FetchOp
                (Button.button
                    [ Button.outlinePrimary
                    , Button.attrs [ Spacing.ml1, onClick Submit ]
                    ]
                    [ text "Fetch" ]
                )
            ]
        , Card.group
            [ Card.config []
                |> Card.header [] [ text "Income" ]
                |> Card.block [] [ Block.text [] [ text (String.fromFloat totalIncome) ] ]
            , Card.config []
                |> Card.header [] [ text "Expenses" ]
                |> Card.block [] [ Block.text [] [ text (String.fromFloat totalExpenses) ] ]
            ]
        , categoryTableView model.summaryPrivate.transactions
        ]
