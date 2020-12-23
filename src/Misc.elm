module Misc exposing (bootstrapIcon)

import Html exposing (Html)
import Svg
import Svg.Attributes as Svgattr


bootstrapIcon : String -> Html msg
bootstrapIcon name =
    Svg.svg
        [ Svgattr.class "bi", Svgattr.fill "currentColor", Svgattr.height "1.5rem", Svgattr.width "1.5rem" ]
        [ Svg.use [ Svgattr.xlinkHref ("/bootstrap-icons-1.0.0/bootstrap-icons.svg#" ++ name) ] [] ]
