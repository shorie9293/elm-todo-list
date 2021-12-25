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
import Json.Decode.Pipeline exposing (hardcoded, required, optional)
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

-- VIEW

viewEnemy : Model -> Html Msg
viewEnemy model =
  div []
      [ text ("Enemy HP: " ++ String.fromInt model.enemyHp) ]
  

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
    [ ("enemyHp", Encode.int model.enemyHp)
    , ("lastEnemyHp", Encode.int model.lastEnemyHp)
    , ("exp", Encode.int model.exp)
    , ("level", Encode.int model.level)
    , ("levelFlag", Encode.bool model.levelFlag)
    , ("point", Encode.int model.point)
    , ("attack", Encode.int model.attack)
    ]

statusDecoder : Decode.Decoder Model
statusDecoder =
  Decode.succeed Model
    |> required "enemyHp" Decode.int
    |> required "lastEnemyHp" Decode.int
    |> required "exp" Decode.int
    |> required "level" Decode.int
    |> required "levelFlag" Decode.bool
    |> required "point" Decode.int
    |> required "attack" Decode.int
