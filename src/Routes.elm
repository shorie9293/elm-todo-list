module Routes exposing (Route(..), href, match, routeToUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)
import Html
import Html.Attributes


type Route
  = Todo
  | Buttle


routes : Parser (Route -> a) a
routes =
  Parser.oneOf
    [ Parser.map Todo Parser.top
    , Parser.map Buttle (Parser.s "buttle")
    ]

match : Url -> Maybe Route
match url =
  Parser.parse routes url

routeToUrl : Route -> String
routeToUrl route =
  case route of
    Todo ->
      "/"
    Buttle ->
      "/buttle"
     
href : Route -> Html.Attribute msg
href route =
  Html.Attributes.href (routeToUrl route)
