module StatusTests exposing (attackToEnemyTests)

import TodoMain as Main exposing (Model, Page, reduceEnemyHp, encountNextEnemy, judgeLevelUp)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Browser.Navigation as Navigation
import Browser.Dom exposing (Error(..))


type Page
    = Todo
    | Buttle
    | NotFound

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

type alias ButtleModel =
  { enemy : EnemyModel
  , actor : ActorModel
  }

type alias Model =
  { page : Page
  , navigationKey : Navigation.Key
  , buttle : ButtleModel
  }


attackToEnemyTests : Test
attackToEnemyTests =
    let

        enemyStatus = 
            { id = 0
            , enemyHp = 10
            , lastEnemyHp = 10 }

        actorStatus =
            { id = 0
            , exp = 0
            , level = 1
            , point = 0
            , attack = 1
            , levelFlag = False }

        status =
            { enemy = enemyStatus
            , actor = actorStatus
            }


    in
    describe "敵に攻撃する"
        [ test "敵のHPを初期値からAttack分減らす" <|
            \_ ->
                reduceEnemyHp status
                |> Expect.equal { status | enemy = {enemyStatus | enemyHp = 9}}
        , test "敵のHPが0以下の時、前回のHPから5増やす。" <|
            \_ ->
                encountNextEnemy { enemy = {enemyStatus | enemyHp = 0}, actor = actorStatus }
                |> Expect.equal { enemy = {enemyStatus | enemyHp = 15, lastEnemyHp = 15}, actor = {actorStatus | exp = 10, levelFlag = True} }
        , test "経験値が30のときLevelを1, attackを5あげる" <|
            \_ ->
                judgeLevelUp  { enemy = {enemyStatus | enemyHp = 0}, actor = {actorStatus | exp = 30, levelFlag = True} }
                |> Expect.equal { enemy = {enemyStatus | enemyHp = 0}, actor = {actorStatus | exp = 30, attack = 6, level = 2, levelFlag = False} }
        ]        
