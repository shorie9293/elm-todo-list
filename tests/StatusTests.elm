module StatusTests exposing (attackToEnemyTests)

import TodoMain exposing (reduceEnemyHp, encountNextEnemy, judgeLevelUp)
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
        
    in
    describe "敵に攻撃する"
        [ test "敵のHPを初期値からAttack分減らす" <|
            \_ ->
                reduceEnemyHp status
                |> Expect.equal { enemy = {enemyStatus | enemyHp = 9}, actor = actorStatus }
        , test "敵のHPが0以下の時、前回のHPから5増やす。" <|
            \_ ->
                encountNextEnemy { enemy = {enemyStatus | enemyHp = 0}, actor = actorStatus }
                |> Expect.equal { enemy = {enemyStatus | enemyHp = 15, lastEnemyHp = 15}, actor = {actorStatus | exp = 10, levelFlag = True} }
        , test "経験値が30のときLevelを1, attackを5あげる" <|
            \_ ->
                judgeLevelUp  { enemy = {enemyStatus | enemyHp = 0}, actor = {actorStatus | exp = 30, levelFlag = True} }
                |> Expect.equal { enemy = {enemyStatus | enemyHp = 0}, actor = {actorStatus | exp = 30, attack = 6, level = 2, levelFlag = False} }
        ]        
