module StatusTests exposing (attackToEnemyTests)

import TodoMain exposing (
    initTodoModel
  , reduceEnemyHp
  , encountNextEnemy
  , judgeLevelUp)
import Routes as R exposing (Route(..), routeToUrl)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

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
    
    initTodoModel =
      [ 
        { id = 0
        , checked = False
        , task = ""
        , project = "" 
        , todoType = ""
        }
      ]
  in
  describe "全てのテスト" [
    describe "バトルフィールドのテスト"
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
  , describe "Router Test"
      [ describe "URLのルートの引数にRoute型を入れると該当の引数が戻ってくる"
        [
          test "Route: Todo" <|
            \_ ->
            routeToUrl R.Todo
            |> Expect.equal "/"
        ,  test "Route: " <|
            \_ ->
            routeToUrl R.Buttle
            |> Expect.equal "/buttle"
        ]
      ]
  , describe "Todo Test"
      [ test " id=0, checked=false, todoの初期値がタイトル→空、重要度→メイン、種類→つぎにやる" <|
          \_ ->
            initTodoModel
            |> Expect.equal initTodoModel

      ]
  -- , describe "Event"
  --     [ test "Ctrl+Enterを押したときにmessage(string)を返す" <|
  --         \_ ->
  --           onCtrEnter "test"
  --           |> Expect.equal "clicked on Ctr Enter button"

  --     ]

  ]
