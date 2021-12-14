module TodoMain exposing (..)

import Browser
import Html exposing ( Html, div, text )
import Html.Attributes exposing ( class )
-- MAIN
main : Program () Model Msg
main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }

-- MODEL
type alias Model =
  { todo : String}

init : () -> (Model, Cmd Msg)
init _ = 
  ({ todo = "first todo!!" }, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
    div [ class "todo-text"]
      [ text model.todo ]

-- UPDATE
type Msg = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
           (model, Cmd.none)
