module StatusTests exposing (attackToEnemyTests)

import TodoMain exposing (init, initModel, reduceEnemyHp, encountNextEnemy, judgeLevelUp)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)



attackToEnemyTests : Test
attackToEnemyTests =
    let
        status = 
            { enemyHp = 10
            , lastEnemyHp = 10
            , exp = 0
            , level = 1
            , point = 0
            , attack = 1
            , levelFlag = False }
        
    in
    describe "敵に攻撃する"
        [ test "敵のHPを初期値からAttack分減らす" <|
            \_ ->
                reduceEnemyHp status
                |> Expect.equal { status | enemyHp = 9 }
        , test "敵のHPが0以下の時、前回のHPから5増やす。" <|
            \_ ->
                encountNextEnemy  { status | enemyHp = 0 }
                |> Expect.equal { status | enemyHp = 15, exp = 10, lastEnemyHp = 15, levelFlag = True }
        , test "経験値が30のときLevelを1, attackを5あげる" <|
            \_ ->
                judgeLevelUp  { status | enemyHp = 0, lastEnemyHp = 10, exp = 30, level = 1, levelFlag = True }
                |> Expect.equal { status | enemyHp = 0, exp = 30, level = 2, attack = 6, levelFlag = False }
        , test "statusとinitModelが一致する" <|
            \_ ->
                Expect.equal status initModel
        , test "flagに15を渡してinitのHPに渡し、statusとinitが一致する" <|
            \_ ->
                Expect.equal ({status | enemyHp = 15}, Cmd.none) (init 15)
        ]        
