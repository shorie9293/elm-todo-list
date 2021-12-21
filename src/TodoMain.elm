module TodoMain exposing (initModel, reduceEnemyHp)

import Browser
import Html exposing ( Html, div, text )
import Html.Attributes exposing ( class )
import Html exposing (button)
import Html.Events exposing (onClick)
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
  { enemyHp : Int
  , exp : Int
  , level : Int
  , point : Int
  , attack : Int}

initModel : Model
initModel =
  { enemyHp = 10
  , exp = 0
  , level = 1
  , point = 0
  , attack = 2
  }


init : () -> (Model, Cmd Msg)
init _ = 
  ( initModel  
  , Cmd.none
  )

-- VIEW

viewEnemy : Model -> Html Msg
viewEnemy model =
  div []
      [ text ("Enemy HP: " ++ String.fromInt model.enemyHp) ]
  


-- UPDATE
type Msg = AttackToEnemy

reduceEnemyHp : Model -> Int
reduceEnemyHp model =
    model.enemyHp - model.attack

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AttackToEnemy ->
          ({ model | enemyHp = reduceEnemyHp model }, Cmd.none)
viewActor : Model -> Html Msg
viewActor model =
  div []
      [ div [] 
            [ text ("LV: " ++ String.fromInt model.level) ]    
      , div [] 
            [ text ("EXP: " ++ String.fromInt model.exp) ]
      , div [] 
            [ text ("Point: " ++ String.fromInt model.point) ]
      , div [] 
            [ text ("Attack: " ++ String.fromInt model.attack) ]
      , button [onClick AttackToEnemy] [ text "Attack" ]        
      ]
view : Model -> Html Msg
view model =
  div [] 
      [ viewEnemy model
      , viewActor model  
      ]
