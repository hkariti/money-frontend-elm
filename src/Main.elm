module Main exposing (..)

import Account
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation as Nav
import Categories
import Html exposing (..)
import Html.Attributes exposing (..)
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
    { transactions : Transactions.Model
    , accounts : Account.Model
    , categories : Categories.Model
    , key : Nav.Key
    , navState : Navbar.State
    , page : Page
    }


type Page
    = NotFound
    | Home
    | TransPage
    | AccountsPage
    | CategoriesPage


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( transModel, transCmd ) =
            Transactions.init ()

        ( accModel, accCmd ) =
            Account.init ()

        ( catModel, catCmd ) =
            Categories.init ()

        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        page =
            urlUpdate url
    in
    ( Model transModel accModel catModel key navState page
    , Cmd.batch
        [ Cmd.map ToTransaction transCmd
        , navCmd
        , Cmd.map ToAccount
            accCmd
        , Cmd.map ToCategory
            catCmd
        ]
    )


type Msg
    = ToTransaction Transactions.Msg
    | ToAccount Account.Msg
    | ToCategory Categories.Msg
    | MsgNone
    | NavMsg Navbar.State
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


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
                    Transactions.update t model.transactions
            in
            ( { model | transactions = newModel }, Cmd.map ToTransaction cmd )

        ToAccount t ->
            let
                ( newModel, cmd ) =
                    Account.update t model.accounts
            in
            ( { model | accounts = newModel }, Cmd.map ToAccount cmd )

        ToCategory t ->
            let
                ( newModel, cmd ) =
                    Categories.update t model.categories
            in
            ( { model | categories = newModel }, Cmd.map ToCategory cmd )

        MsgNone ->
            ( model, Cmd.none )

        NavMsg a ->
            ( { model | navState = a }, Cmd.none )


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
        [ UrlParser.map Home top
        , UrlParser.map TransPage (UrlParser.s "transactions")
        , UrlParser.map AccountsPage (UrlParser.s "accounts")
        , UrlParser.map CategoriesPage (UrlParser.s "categories")
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Test"
    , body =
        [ div []
            [ CDN.stylesheet
            , menu model
            , mainContent model
            ]
        ]
    }


menu model =
    Navbar.config NavMsg
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "Elm Bootstrap" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#transactions" ] [ text "Transactions" ]
            , Navbar.itemLink [ href "#accounts" ] [ text "Accounts" ]
            , Navbar.itemLink [ href "#categories" ] [ text "Categories" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    case model.page of
        NotFound ->
            text "NOT Found!"

        Home ->
            text "Home"

        TransPage ->
            Html.map ToTransaction (Transactions.view model.transactions)

        AccountsPage ->
            Html.map ToAccount (Account.view model.accounts)

        CategoriesPage ->
            Html.map ToCategory (Categories.view model.categories)
