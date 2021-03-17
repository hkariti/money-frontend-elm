module Patterns exposing (Msg, PatternPrivate, init, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Categories exposing (Category)
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


type alias Field =
    String


type MatcherValue
    = Eq String
    | Lt Float
    | Le Float
    | Gt Float
    | Ge Float
    | Regex String
    | Isnull Bool


type alias Matcher =
    { field : Field
    , matcher : List MatcherValue
    }


type MatcherCombination
    = Match Matcher
    | And (List MatcherCombination)
    | Or (List MatcherCombination)
    | Not MatcherCombination


type alias Pattern =
    { id : Int
    , name : Maybe String
    , target_category : String
    , matcher : MatcherCombination
    , enabled : Bool
    }


type alias PatternForm =
    { name : String
    , target_category : String
    , matcher : String
    , enabled : Bool
    }


type Operation
    = UpdateOp
    | DeleteOp
    | FetchOp


type alias PatternPrivate =
    { edit : Maybe Int
    , form : PatternForm
    , currentOp : Maybe Operation
    , patterns : List Pattern
    }


type alias PatternModel m =
    { m
        | patternPrivate : PatternPrivate
        , message : Message.Model
        , categories : List Category
    }


init : () -> ( PatternPrivate, Cmd Msg )
init _ =
    ( PatternPrivate Nothing emptyForm (Just FetchOp) [], getPatternsCmd )


emptyForm : PatternForm
emptyForm =
    PatternForm "" "" "" False


getPatternsCmd =
    Http.get
        { url = "http://localhost:8000/pattern/"
        , expect = Http.expectJson GotPatterns (D.list patternParser)
        }


addPatternCmd : PatternForm -> Cmd Msg
addPatternCmd form =
    Http.post
        { url = "http://localhost:8000/pattern/"
        , expect = Http.expectJson GotResponseAdd patternParser
        , body = Http.jsonBody (patternEncoder Nothing form)
        }


delPatternCmd : Int -> Cmd Msg
delPatternCmd id =
    Http.request
        { method = "DELETE"
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = "http://localhost:8000/pattern/" ++ String.fromInt id ++ "/"
        , expect = Http.expectWhatever GotResponseDelete
        }


updatePatternCmd : Int -> PatternForm -> Cmd Msg
updatePatternCmd id form =
    Http.request
        { method = "PUT"
        , headers = []
        , body = Http.jsonBody (patternEncoder (Just id) form)
        , timeout = Nothing
        , tracker = Nothing
        , url = "http://localhost:8000/pattern/" ++ String.fromInt id ++ "/"
        , expect = Http.expectJson GotResponseUpdate patternParser
        }


matcherValueParser : D.Decoder (List MatcherValue)
matcherValueParser =
    D.map7
        (\a b c d e f g -> MExtra.values [ a, b, c, d, e, f, g ])
        (D.maybe (D.field "eq" (D.map Eq D.string)))
        (D.maybe (D.field "lt" (D.map Lt D.float)))
        (D.maybe (D.field "le" (D.map Le D.float)))
        (D.maybe (D.field "gt" (D.map Gt D.float)))
        (D.maybe (D.field "ge" (D.map Ge D.float)))
        (D.maybe (D.field "regex" (D.map Regex D.string)))
        (D.maybe (D.field "isnull" (D.map Isnull D.bool)))


matcherParser : D.Decoder Matcher
matcherParser =
    D.map2 Matcher
        (D.field "field" D.string)
        matcherValueParser


matcherCombinationParser : D.Decoder MatcherCombination
matcherCombinationParser =
    D.oneOf
        [ D.field "not" (D.map Not (D.lazy (\_ -> matcherCombinationParser)))
        , D.field "and" (D.map And (D.list (D.lazy (\_ -> matcherCombinationParser))))
        , D.field "or" (D.map Or (D.list (D.lazy (\_ -> matcherCombinationParser))))
        , D.map Match matcherParser
        ]


patternParser : D.Decoder Pattern
patternParser =
    D.map5 Pattern
        (D.field "id" D.int)
        (D.maybe (D.field "name" D.string))
        (D.field "target_category" D.string)
        (D.field "matcher" matcherCombinationParser)
        (D.field "enabled" D.bool)


matcherValueEncoder : MatcherValue -> ( String, E.Value )
matcherValueEncoder v =
    case v of
        Eq a ->
            ( "eq", E.string a )

        Lt a ->
            ( "lt", E.float a )

        Le a ->
            ( "le", E.float a )

        Gt a ->
            ( "gt", E.float a )

        Ge a ->
            ( "ge", E.float a )

        Regex a ->
            ( "regex", E.string a )

        Isnull a ->
            ( "isnull", E.bool a )


matcherEncoder : Matcher -> E.Value
matcherEncoder m =
    E.object
        ([ ( "field", E.string m.field ) ]
            ++ (m.matcher |> List.map matcherValueEncoder)
        )


matcherCombinationEncoder : MatcherCombination -> E.Value
matcherCombinationEncoder m =
    case m of
        Not a ->
            E.object [ ( "not", matcherCombinationEncoder a ) ]

        And a ->
            E.object [ ( "and", E.list matcherCombinationEncoder a ) ]

        Or a ->
            E.object [ ( "or", E.list matcherCombinationEncoder a ) ]

        Match a ->
            matcherEncoder a


patternEncoder : Maybe Int -> PatternForm -> E.Value
patternEncoder maybe_id p =
    let
        matcherCombination : Result D.Error MatcherCombination
        matcherCombination =
            D.decodeString matcherCombinationParser p.matcher
    in
    E.object
        [ ( "id", Maybe.map E.int maybe_id |> Maybe.withDefault E.null )
        , ( "name"
          , if String.isEmpty p.name then
                E.null

            else
                E.string p.name
          )
        , ( "enabled", E.bool p.enabled )
        , ( "matcher", matcherCombination |> Result.map matcherCombinationEncoder |> Result.withDefault E.null )
        , ( "target_category", E.string p.target_category )
        ]



--
--
--
---- UPDATE
--
--


type Msg
    = GotPatterns (Result Http.Error (List Pattern))
    | GotResponseAdd (Result Http.Error Pattern)
    | GotResponseDelete (Result Http.Error ())
    | GotResponseUpdate (Result Http.Error Pattern)
    | Add
    | Remove Int
    | Edit Int
    | EditName String
    | EditMatcher String
    | EditCategory String
    | SetEnabled Bool
    | AcceptEdit
    | CancelEdit


updateFromResult : PatternModel m -> Result Http.Error a -> (a -> ( PatternModel m, Cmd Msg )) -> ( PatternModel m, Cmd Msg )
updateFromResult model res f =
    let
        oldPrivate =
            model.patternPrivate
    in
    case res of
        Ok a ->
            f a

        Err (Http.BadBody e) ->
            ( { model | message = Message.danger e, patternPrivate = { oldPrivate | currentOp = Nothing } }, Cmd.none )

        Err _ ->
            ( { model
                | message = Message.danger "Error!"
                , patternPrivate =
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
    { m | form : PatternForm }


type alias PrivatePatterns m =
    { m | patterns : List Pattern }


update : Msg -> PatternModel m -> ( PatternModel m, Cmd Msg )
update msg model =
    let
        oldForm =
            model.patternPrivate.form

        updateOp : Maybe Operation -> PrivateOp PatternPrivate -> PrivateOp PatternPrivate
        updateOp o p =
            { p | currentOp = o }

        updateEdit : Maybe Int -> PrivateEdit PatternPrivate -> PrivateEdit PatternPrivate
        updateEdit o p =
            { p | edit = o }

        updateForm : PatternForm -> PrivateForm PatternPrivate -> PrivateForm PatternPrivate
        updateForm o p =
            { p | form = o }

        updatePatterns : List Pattern -> PrivatePatterns PatternPrivate -> PrivateOp PatternPrivate
        updatePatterns o p =
            { p | patterns = o }
    in
    case msg of
        GotPatterns res ->
            (\a -> ( { model | patternPrivate = model.patternPrivate |> updateOp Nothing |> updatePatterns a }, Cmd.none ))
                |> updateFromResult model res

        GotResponseAdd res ->
            (\a ->
                ( { model
                    | message = Message.info "Added pattern"
                    , patternPrivate = model.patternPrivate |> updateOp Nothing |> updateEdit Nothing |> updatePatterns (a :: model.patternPrivate.patterns)
                  }
                , Cmd.none
                )
            )
                |> updateFromResult model res

        GotResponseDelete res ->
            case model.patternPrivate.edit of
                Nothing ->
                    ( model, Cmd.none )

                Just i ->
                    (\_ ->
                        ( { model
                            | message = Message.info "Deleted pattern"
                            , patternPrivate = model.patternPrivate |> updateOp Nothing |> updateEdit Nothing |> updatePatterns (removeAt i model.patternPrivate.patterns)
                          }
                        , Cmd.none
                        )
                    )
                        |> updateFromResult model res

        GotResponseUpdate res ->
            case model.patternPrivate.edit of
                Nothing ->
                    ( model, Cmd.none )

                Just i ->
                    (\a ->
                        ( { model
                            | message = Message.info "Updated pattern"
                            , patternPrivate = model.patternPrivate |> updateOp Nothing |> updateEdit Nothing |> updatePatterns (setAt i a model.patternPrivate.patterns)
                          }
                        , Cmd.none
                        )
                    )
                        |> updateFromResult model res

        Add ->
            ( { model | patternPrivate = model.patternPrivate |> updateForm emptyForm |> updateEdit (Just -1) }, Cmd.none )

        Remove i ->
            case getAt i model.patternPrivate.patterns of
                Just pattern ->
                    ( { model | patternPrivate = model.patternPrivate |> updateOp (Just DeleteOp) }
                    , delPatternCmd pattern.id
                    )

                Nothing ->
                    ( model, Cmd.none )

        Edit i ->
            let
                entry : Maybe Pattern
                entry =
                    getAt i model.patternPrivate.patterns
            in
            case entry of
                Just e ->
                    ( { model
                        | patternPrivate =
                            model.patternPrivate
                                |> updateEdit (Just i)
                                |> updateForm (PatternForm (e.name |> Maybe.withDefault "") e.target_category (E.encode 0 (matcherCombinationEncoder e.matcher)) e.enabled)
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        EditName v ->
            ( { model | patternPrivate = model.patternPrivate |> updateForm { oldForm | name = v } }, Cmd.none )

        EditMatcher v ->
            ( { model | patternPrivate = model.patternPrivate |> updateForm { oldForm | matcher = v } }, Cmd.none )

        EditCategory v ->
            ( { model | patternPrivate = model.patternPrivate |> updateForm { oldForm | target_category = v } }, Cmd.none )

        SetEnabled v ->
            ( { model | patternPrivate = model.patternPrivate |> updateForm { oldForm | enabled = v } }, Cmd.none )

        AcceptEdit ->
            case model.patternPrivate.edit of
                Nothing ->
                    ( model, Cmd.none )

                Just i ->
                    if not (validateForm model.patternPrivate.form) then
                        ( model, Cmd.none )

                    else if i == -1 then
                        -- New row
                        ( { model | patternPrivate = model.patternPrivate |> updateOp (Just UpdateOp) }
                        , addPatternCmd model.patternPrivate.form
                        )

                    else
                        case getAt i model.patternPrivate.patterns of
                            Nothing ->
                                ( { model
                                    | patternPrivate =
                                        model.patternPrivate
                                            |> updateForm emptyForm
                                            |> updateEdit Nothing
                                  }
                                , Cmd.none
                                )

                            Just currentEntry ->
                                ( { model | patternPrivate = model.patternPrivate |> updateOp (Just UpdateOp) }
                                , updatePatternCmd currentEntry.id model.patternPrivate.form
                                )

        CancelEdit ->
            ( { model | patternPrivate = model.patternPrivate |> updateForm emptyForm |> updateEdit Nothing }, Cmd.none )


validateForm : PatternForm -> Bool
validateForm form =
    let
        isEmpty =
            String.trim >> String.isEmpty
    in
    case D.decodeString matcherCombinationParser form.matcher of
        Err _ ->
            False

        Ok _ ->
            if isEmpty form.target_category then
                False

            else
                True



-- VIEW


view : PatternModel m -> Html Msg
view model =
    let
        isEditing : Int -> Maybe Int
        isEditing i =
            MExtra.filter ((==) i) model.patternPrivate.edit

        spinnerIfOp : Operation -> Html msg -> Html msg
        spinnerIfOp o e =
            MExtra.filter ((==) o) model.patternPrivate.currentOp
                |> Maybe.map
                    (\_ ->
                        Button.button [ Button.outlinePrimary, Button.attrs [] ]
                            [ Spinner.spinner [ Spinner.small, Spinner.attrs [ Spacing.m1 ] ] []
                            ]
                    )
                |> Maybe.withDefault e

        editRow : Pattern -> Int -> Table.Row Msg
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
                    [ Checkbox.checkbox
                        [ Checkbox.onCheck SetEnabled
                        , Checkbox.checked model.patternPrivate.form.enabled
                        ]
                        ""
                    ]
                , Table.td []
                    [ Input.text
                        [ Input.attrs
                            [ value model.patternPrivate.form.name
                            , onInput EditName
                            , disabled (MExtra.isJust model.patternPrivate.currentOp)
                            ]
                        ]
                    ]
                , Table.td []
                    [ Input.text
                        [ Input.attrs
                            [ value model.patternPrivate.form.target_category
                            , onInput EditCategory
                            , disabled (MExtra.isJust model.patternPrivate.currentOp)
                            ]
                        ]
                    ]
                , Table.td []
                    [ Input.text
                        [ Input.attrs
                            [ value model.patternPrivate.form.matcher
                            , onInput EditMatcher
                            , disabled (MExtra.isJust model.patternPrivate.currentOp)
                            ]
                        ]
                    ]
                , Table.td []
                    [ spinnerIfOp UpdateOp
                        (Button.button
                            [ Button.outlinePrimary
                            , Button.attrs
                                [ disabled (MExtra.isJust model.patternPrivate.currentOp)
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
                            , disabled (MExtra.isJust model.patternPrivate.currentOp)
                            ]
                        ]
                        [ bootstrapIcon "x" ]
                    , spinnerIfOp DeleteOp
                        (Button.button
                            [ Button.outlinePrimary
                            , Button.attrs
                                [ Spacing.ml1
                                , onClick (Remove i)
                                , disabled (MExtra.isJust model.patternPrivate.currentOp)
                                ]
                            ]
                            [ bootstrapIcon "trash" ]
                        )
                    ]
                ]

        displayRow : Pattern -> Int -> Table.Row Msg
        displayRow a i =
            Table.tr []
                [ Table.td [] [ text (String.fromInt a.id) ]
                , Table.td [] [ input [ type_ "checkbox", checked a.enabled ] [] ]
                , Table.td [] [ text (a.name |> Maybe.withDefault "") ]
                , Table.td [] [ text a.target_category ]
                , Table.td [] [ text (E.encode 0 (matcherCombinationEncoder a.matcher)) ]
                , Table.td []
                    [ Button.button
                        [ Button.outlinePrimary
                        , Button.attrs [ disabled (MExtra.isJust model.patternPrivate.currentOp), Spacing.ml1, onClick (Edit i) ]
                        ]
                        [ bootstrapIcon "pencil" ]
                    ]
                ]

        toRow : Int -> Pattern -> Table.Row Msg
        toRow i a =
            isEditing i |> Maybe.map (editRow a) |> Maybe.withDefault (displayRow a i)

        addRow : Maybe (Table.Row Msg)
        addRow =
            isEditing -1 |> Maybe.map (editRow (Pattern -1 Nothing "" (Match (Matcher "asd" [ Eq "" ])) False))
    in
    div []
        [ CDN.stylesheet
        , Button.button
            [ Button.outlinePrimary
            , Button.attrs
                [ Spacing.m1
                , onClick Add
                , disabled (MExtra.isJust model.patternPrivate.currentOp)
                ]
            ]
            [ bootstrapIcon "plus" ]
        , Table.simpleTable
            ( Table.simpleThead
                [ Table.th [] [ text "ID" ]
                , Table.th [] [ text "Enabled" ]
                , Table.th [] [ text "Name" ]
                , Table.th [] [ text "Target Category" ]
                , Table.th [] [ text "Matcher" ]
                ]
            , Table.tbody [] (MExtra.cons addRow (List.indexedMap toRow model.patternPrivate.patterns))
            )
        ]
