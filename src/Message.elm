module Message exposing (Model, danger, hide, info, setVisibility, view, warn)

import Bootstrap.Alert as Alert
import Html exposing (Html, text)


type Level
    = Info
    | Warn
    | Danger


type alias Model =
    { visibility : Alert.Visibility
    , level : Level
    , text : String
    }


new : Level -> (String -> Model)
new l =
    Model Alert.shown l


info : String -> Model
info =
    new Info


warn : String -> Model
warn =
    new Warn


danger : String -> Model
danger =
    new Danger


hide : Model
hide =
    Model Alert.closed Info ""


levelFunc : Level -> (Alert.Config msg -> Alert.Config msg)
levelFunc l =
    case l of
        Info ->
            Alert.info

        Warn ->
            Alert.warning

        Danger ->
            Alert.danger


setVisibility : Model -> Alert.Visibility -> Model
setVisibility m v =
    { m | visibility = v }


view : (Alert.Visibility -> msg) -> Model -> Html msg
view m model =
    Alert.config
        |> Alert.dismissable m
        |> levelFunc model.level
        |> Alert.children [ text model.text ]
        |> Alert.view model.visibility
