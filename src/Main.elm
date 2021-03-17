module Main exposing (..)

import Account
import Bootstrap.Alert as Alert
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation as Nav
import Categories
import Html exposing (..)
import Html.Attributes exposing (..)
import Message
import Patterns
import Summary
import Transactions
import Url
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { transactions : List Transactions.Transaction
    , transactionPrivate : Transactions.TransactionPrivate
    , accounts : List Account.Account
    , accountPrivate : Account.AccountPrivate
    , categories : List Categories.Category
    , categoryPrivate : Categories.CategoryPrivate
    , patternPrivate : Patterns.PatternPrivate
    , summaryPrivate : Summary.SummaryPrivate
    , message : Message.Model
    , key : Nav.Key
    , navState : Navbar.State
    , page : Page
    }


type Page
    = NotFound
    | SummaryPage
    | TransPage
    | AccountsPage
    | CategoriesPage
    | PatternsPage


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( summaryPrivate, sumCmd ) =
            Summary.init ()

        ( transactionPrivate, transCmd ) =
            Transactions.init ()

        ( accountPrivate, accCmd ) =
            Account.init ()

        ( categoryPrivate, catCmd ) =
            Categories.init ()

        ( patternPrivate, patCmd ) =
            Patterns.init ()

        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        page =
            urlUpdate url
    in
    ( Model [] transactionPrivate [] accountPrivate [] categoryPrivate patternPrivate summaryPrivate Message.hide key navState page
    , Cmd.batch
        [ Cmd.map ToTransaction transCmd
        , navCmd
        , Cmd.map ToAccount
            accCmd
        , Cmd.map ToCategory
            catCmd
        , Cmd.map ToPattern
            patCmd
        , Cmd.map ToSummary sumCmd
        ]
    )


type Msg
    = ToTransaction Transactions.Msg
    | ToAccount Account.Msg
    | ToCategory Categories.Msg
    | ToPattern Patterns.Msg
    | ToSummary Summary.Msg
    | MsgNone
    | NavMsg Navbar.State
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AlertMessage Alert.Visibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | page = urlUpdate url }, Cmd.none )

        ToTransaction t ->
            let
                ( newModel, cmd ) =
                    Transactions.update t model
            in
            ( newModel, Cmd.map ToTransaction cmd )

        ToAccount t ->
            let
                ( newModel, cmd ) =
                    Account.update t model
            in
            ( newModel, Cmd.map ToAccount cmd )

        ToCategory t ->
            let
                ( newModel, cmd ) =
                    Categories.update t model
            in
            ( newModel, Cmd.map ToCategory cmd )

        ToPattern t ->
            let
                ( newModel, cmd ) =
                    Patterns.update t model
            in
            ( newModel, Cmd.map ToPattern cmd )

        ToSummary t ->
            let
                ( newModel, cmd ) =
                    Summary.update t model
            in
            ( newModel, Cmd.map ToSummary cmd )

        MsgNone ->
            ( model, Cmd.none )

        NavMsg a ->
            ( { model | navState = a }, Cmd.none )

        AlertMessage v ->
            ( { model | message = Message.setVisibility model.message v }, Cmd.none )


urlUpdate : Url.Url -> Page
urlUpdate url =
    case decode url of
        Nothing ->
            NotFound

        Just route ->
            route


decode : Url.Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map SummaryPage top
        , UrlParser.map SummaryPage (UrlParser.s "summary")
        , UrlParser.map TransPage (UrlParser.s "transactions")
        , UrlParser.map AccountsPage (UrlParser.s "accounts")
        , UrlParser.map CategoriesPage (UrlParser.s "categories")
        , UrlParser.map PatternsPage (UrlParser.s "patterns")
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Test"
    , body =
        [ div []
            [ CDN.stylesheet
            , menu model
            , Message.view AlertMessage model.message
            , mainContent model
            ]
        ]
    }


menu model =
    Navbar.config NavMsg
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "Elm Bootstrap" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#summary" ] [ text "Summary" ]
            , Navbar.itemLink [ href "#transactions" ] [ text "Transactions" ]
            , Navbar.itemLink [ href "#accounts" ] [ text "Accounts" ]
            , Navbar.itemLink [ href "#categories" ] [ text "Categories" ]
            , Navbar.itemLink [ href "#patterns" ] [ text "Patterns" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    case model.page of
        NotFound ->
            text "NOT Found!"

        SummaryPage ->
            Html.map ToSummary (Summary.view model)

        TransPage ->
            Html.map ToTransaction (Transactions.view model)

        AccountsPage ->
            Html.map ToAccount (Account.view model)

        CategoriesPage ->
            Html.map ToCategory (Categories.view model)

        PatternsPage ->
            Html.map ToPattern (Patterns.view model)
