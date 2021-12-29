port module TodoMain exposing ( main
                          , reduceEnemyHp
                          , encountNextEnemy
                          , judgeLevelUp )

import Browser
import Html exposing ( Html, div, text )
import Html.Attributes exposing ( class )
import Html exposing (button)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode

-- MAIN
main : Program Encode.Value Model Msg
main = Browser.element
  { init = init
  , view = view
  , update = updateWithStorage
  , subscriptions = \_ -> Sub.none
  }


-- MODEL

type alias EnemyModel =
  { id : Int
  , enemyHp : Int
  , lastEnemyHp : Int
  }

type alias ActorModel =
  { id : Int
  , exp : Int
  , level : Int
  , levelFlag : Bool
  , point : Int
  , attack : Int}

type alias Model =
  { enemy : EnemyModel
  , actor : ActorModel
  }


initEnemyModel : EnemyModel
initEnemyModel =
  { id = 0
  , enemyHp = 10
  , lastEnemyHp = 10
  }


initActorModel : ActorModel
initActorModel =
  { id = 0
  , exp = 0
  , level = 1
  , levelFlag = False
  , point = 0
  , attack = 1
  }
    
initModel : Model
initModel =
  { enemy = initEnemyModel
  , actor = initActorModel
  }


init : Encode.Value -> (Model, Cmd Msg)
init flags = 
  ( case Decode.decodeValue statusDecoder flags of
      Ok model -> model
      Err _ -> initModel
  , Cmd.none )

  
-- UPDATE
type Msg = AttackToEnemy

attackToEnemy : Model -> Model
attackToEnemy model =
  reduceEnemyHp model
  |> encountNextEnemy
  |> judgeLevelUp

reduceEnemyHp : Model -> Model
reduceEnemyHp model =
  let
    oldEnemyModel = model.enemy
    oldActorModel = model.actor
    newEnemyModel = { oldEnemyModel | enemyHp = oldEnemyModel.enemyHp - oldActorModel.attack }
  in
    { model | enemy = newEnemyModel }
    
encountNextEnemy : Model -> Model
encountNextEnemy model =
  let
    oldEnemyModel = model.enemy
    oldActorModel = model.actor
    newEnemyModel = { oldEnemyModel | enemyHp = oldEnemyModel.lastEnemyHp + 5
                    , lastEnemyHp = oldEnemyModel.lastEnemyHp + 5 }
    newActorModel = { oldActorModel | exp = oldActorModel.exp + 10, levelFlag = True}
  in
  if model.enemy.enemyHp <= 0 then
    { enemy = newEnemyModel, actor = newActorModel }
  else
    model

judgeLevelUp : Model -> Model
judgeLevelUp model =
  let
    oldActorModel = model.actor
    newActorModel = { oldActorModel | level = oldActorModel.level + 1
                    , attack = oldActorModel.attack + 5
                    , levelFlag = False
                    }
  in
  if oldActorModel.levelFlag && modBy 30 oldActorModel.exp == 0 then
    { model | actor = newActorModel }
  else
    { model | actor = { oldActorModel | levelFlag = False } }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AttackToEnemy ->
          (attackToEnemy model, Cmd.none)

-- VIEW

viewEnemy : EnemyModel -> Html Msg
viewEnemy enemyModel =
  div []
      [ text ("Enemy HP: " ++ String.fromInt enemyModel.enemyHp) ]
  

viewActor : ActorModel -> Html Msg
viewActor actorModel =
  div [ class "todo-text" ]
      [ div [] 
            [ text ("LV: " ++ String.fromInt actorModel.level) ]    
      , div [] 
            [ text ("EXP: " ++ String.fromInt actorModel.exp) ]
      , div [] 
            [ text ("Point: " ++ String.fromInt actorModel.point) ]
      , div [] 
            [ text ("Attack: " ++ String.fromInt actorModel.attack) ]
      , button [onClick AttackToEnemy] [ text "Attack" ]        
      ]


view : Model -> Html Msg
view model =
  div [] 
      [ viewEnemy model.enemy
      , viewActor model.actor  
      ]


-- PORT

port setStorage : Encode.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
  let
    ( newModel, cmds ) = update msg oldModel
  in
    ( newModel
    , Cmd.batch [ setStorage (statusEncoder newModel), cmds]
    )

-- JSON ENCODE/DECODE

statusEncoder : Model -> Encode.Value
statusEncoder model =
  Encode.object
    [ ("enemy", Encode.object 
        [ ("id", Encode.int model.enemy.id)
        , ("enemyHp", Encode.int model.enemy.enemyHp)
        , ("lastEnemyHp", Encode.int model.enemy.lastEnemyHp)
        ]
      )
    , ("actor", Encode.object 
        [ ("id", Encode.int model.actor.id)
        , ("exp", Encode.int model.actor.exp)
        , ("level", Encode.int model.actor.level)
        , ("levelFlag", Encode.bool model.actor.levelFlag)
        , ("point", Encode.int model.actor.point)
        , ("attack", Encode.int model.actor.attack)
        ]
      )
    ]

statusEnemyDecoder : Decode.Decoder EnemyModel
statusEnemyDecoder =
  Decode.succeed EnemyModel
    |> required "id" Decode.int
    |> required "enemyHp" Decode.int
    |> required "lastEnemyHp" Decode.int

statusActorDecoder : Decode.Decoder ActorModel
statusActorDecoder =
  Decode.succeed ActorModel
    |> required "id" Decode.int
    |> required "exp" Decode.int
    |> required "level" Decode.int
    |> required "levelFlag" Decode.bool
    |> required "point" Decode.int
    |> required "attack" Decode.int

statusDecoder : Decode.Decoder Model
statusDecoder =
  Decode.map2 Model
    (Decode.field "enemy" statusEnemyDecoder)
    (Decode.field "actor" statusActorDecoder)