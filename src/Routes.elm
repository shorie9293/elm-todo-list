module Routes exposing (Route(..), match)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)

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