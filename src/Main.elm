module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Date exposing (Date, fromIsoString, toIsoString)
import Html exposing (Html, button, div, input, option, select, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (getAt, removeAt, setAt)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



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
    }


type FloatField
    = FloatField (Maybe Float) String


type alias TransactionForm =
    { transaction_date : String
    , bill_date : String
    , original_amount : FloatField
    , billed_amount : FloatField
    , currency : Currency
    }


type alias Model =
    { table : List Transaction
    , form : TransactionForm
    , edit : Maybe Int
    }


init : Model
init =
    { table = []
    , form = emptyForm
    , edit = Nothing
    }


emptyForm =
    { transaction_date = ""
    , bill_date = ""
    , original_amount = FloatField (Just 0) ""
    , billed_amount = FloatField (Just 0) ""
    , currency = "ILS"
    }



-- UPDATE


type Msg
    = Add
    | Remove Int
    | StartEditRow Int
    | SaveEditRow
    | CancelEditRow
    | EditTransDate String
    | EditBillDate String
    | EditTransAmount String
    | EditBillAmount String
    | ChangeCurrency Currency


update : Msg -> Model -> Model
update msg model =
    let
        oldForm =
            model.form
    in
    case msg of
        Add ->
            case createTransaction model.form of
                Just t ->
                    { model | table = model.table ++ [ t ], form = emptyForm }

                Nothing ->
                    model

        Remove index ->
            { model | table = removeAt index model.table }

        StartEditRow n ->
            case getAt n model.table of
                Nothing ->
                    model

                Just r ->
                    { model | edit = Just n, form = toForm r }

        CancelEditRow ->
            { model | edit = Nothing, form = emptyForm }

        SaveEditRow ->
            case createTransaction model.form of
                Just t ->
                    case model.edit of
                        Just i ->
                            { model | table = setAt i t model.table, edit = Nothing, form = emptyForm }

                        Nothing ->
                            model

                Nothing ->
                    model

        EditTransDate d ->
            { model | form = { oldForm | transaction_date = d } }

        EditBillDate d ->
            { model | form = { oldForm | bill_date = d } }

        EditTransAmount a ->
            { model | form = { oldForm | original_amount = FloatField (String.toFloat a) a } }

        EditBillAmount a ->
            { model | form = { oldForm | billed_amount = FloatField (String.toFloat a) a } }

        ChangeCurrency c ->
            { model | form = { oldForm | currency = c } }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            ([ input [ placeholder "Transaction Date", value model.form.transaction_date, onInput EditTransDate ] []
             , input [ placeholder "Bill Date", value model.form.bill_date, onInput EditBillDate ] []
             , floatInput "Original Amount" model.form.original_amount EditTransAmount
             , floatInput "Billed Amount" model.form.billed_amount EditBillAmount
             , select [ onInput ChangeCurrency ] (List.map selectOption currencies)
             ]
                ++ globalActions model.edit
            )
        , table [ style "border-collapse" "collapse" ]
            ([ tr []
                [ th [] [ text "Transaction Date" ]
                , th [] [ text "Bill Date" ]
                , th [] [ text "Original Amount" ]
                , th [] [ text "Billed Amount" ]
                , th [] [ text "Currency" ]
                , th [] [ text "Actions" ]
                ]
             ]
                ++ List.indexedMap (toTableRow model.edit) model.table
            )
        ]


toTableRow : Maybe Int -> Int -> Transaction -> Html Msg
toTableRow edit i t =
    let
        rowStyle =
            case Maybe.map ((==) i) edit of
                Just True ->
                    [ style "border-style" "solid" ]

                _ ->
                    []
    in
    tr rowStyle
        [ td [] [ text (toIsoString t.transaction_date) ]
        , td [] [ text (toIsoString t.bill_date) ]
        , td [] [ text (String.fromFloat t.original_amount) ]
        , td [] [ text (String.fromFloat t.billed_amount) ]
        , td [] [ text t.currency ]
        , td []
            [ button [ onClick (Remove i) ] [ text "Remove" ]
            , button [ onClick (StartEditRow i) ] [ text "Edit" ]
            ]
        ]


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


createTransaction : TransactionForm -> Maybe Transaction
createTransaction form =
    let
        bill_date =
            Result.toMaybe (fromIsoString form.bill_date)

        transaction_date =
            Result.toMaybe (fromIsoString form.transaction_date)

        (FloatField billed_amount _) =
            form.billed_amount

        (FloatField original_amount _) =
            form.original_amount

        newTrans bd td ba oa =
            { transaction_date = td
            , bill_date = bd
            , billed_amount = ba
            , original_amount = oa
            , currency = form.currency
            }
    in
    Maybe.map4 newTrans bill_date transaction_date billed_amount original_amount


toForm : Transaction -> TransactionForm
toForm t =
    { transaction_date = Date.toIsoString t.transaction_date
    , bill_date = Date.toIsoString t.transaction_date
    , original_amount = FloatField (Just t.original_amount) (String.fromFloat t.original_amount)
    , billed_amount = FloatField (Just t.billed_amount) (String.fromFloat t.billed_amount)
    , currency = t.currency
    }


globalActions : Maybe Int -> List (Html Msg)
globalActions e =
    case e of
        Just _ ->
            [ button [ onClick SaveEditRow ] [ text "Save" ]
            , button [ onClick CancelEditRow ] [ text "Cancel" ]
            ]

        Nothing ->
            [ button [ onClick Add ] [ text "Add" ] ]
