module Main exposing (..)

import Account
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation as Nav
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
    , key : Nav.Key
    , navState : Navbar.State
    , page : Page
    }


type Page
    = NotFound
    | Home
    | TransPage
    | AccountsPage


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( transModel, transCmd ) =
            Transactions.init ()

        ( accModel, accCmd ) =
            Account.init ()

        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        page =
            urlUpdate url
    in
    ( Model transModel accModel key navState page, Cmd.batch [ Cmd.map ToTransaction transCmd, navCmd, Cmd.map ToAccount accCmd ] )


type Msg
    = ToTransaction Transactions.Msg
    | ToAccount Account.Msg
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
