module TodoMain exposing ( initModel
                          , reduceEnemyHp
                          , encountNextEnemy
                          , judgeLevelUp )

import Browser
import Html exposing ( Html, div, text )
import Html.Attributes exposing ( class )
import Html exposing (button)
import Html.Events exposing (onClick)
-- import Basics exposing (modBy)


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
  , lastEnemyHp : Int
  , exp : Int
  , level : Int
  , levelFlag : Bool
  , point : Int
  , attack : Int}

initModel : Model
initModel =
  { enemyHp = 10
  , lastEnemyHp = 10
  , exp = 0
  , level = 1
  , levelFlag = False
  , point = 0
  , attack = 1
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

attackToEnemy : Model -> Model
attackToEnemy model =
  reduceEnemyHp model
  |> encountNextEnemy
  |> judgeLevelUp

reduceEnemyHp : Model -> Model
reduceEnemyHp model =
  { model | enemyHp = model.enemyHp - model.attack}

encountNextEnemy : Model -> Model
encountNextEnemy model =
  if model.enemyHp <= 0 then
    { model | enemyHp = model.lastEnemyHp + 5
    , lastEnemyHp = model.lastEnemyHp + 5
    , exp = model.exp + 10
    , levelFlag = True }
  else
    model

judgeLevelUp : Model -> Model
judgeLevelUp model =
  if model.levelFlag && modBy 30 model.exp == 0 then
    { model | level = model.level + 1, attack = model.attack + 5, levelFlag = False }
  else
    { model | levelFlag = False }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AttackToEnemy ->
          (attackToEnemy model, Cmd.none)

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
