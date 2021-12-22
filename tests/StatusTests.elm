module StatusTests exposing (attackToEnemyTests)

import TodoMain exposing (initModel, reduceEnemyHp, encountNextEnemy, judgeLevelUp)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


attackToEnemyTests : Test
attackToEnemyTests =
    describe "敵に攻撃する"
        [ test "敵のHPを初期値からAttack分減らす" <|
            \_ ->
                reduceEnemyHp { enemyHp = 10, lastEnemyHp = 10, exp = 0, level = 1, point = 0, attack = 1, levelFlag = False }
                |> Expect.equal { enemyHp = 9, lastEnemyHp = 10, exp = 0, level = 1, point = 0, attack = 1, levelFlag = False }
        , test "敵のHPが0以下の時、前回のHPから5増やす。" <|
            \_ ->
                encountNextEnemy  { enemyHp = 0, lastEnemyHp = 10, exp = 0, level = 1, point = 0, attack = 1, levelFlag = False }
                |> Expect.equal { enemyHp = 15, lastEnemyHp = 15, exp = 10, level = 1, point = 0, attack = 1, levelFlag = True }
        , test "経験値が30のときLevelを1, attackを5あげる" <|
            \_ ->
                judgeLevelUp  { enemyHp = 0, lastEnemyHp = 10, exp = 30, level = 1, point = 0, attack = 1, levelFlag = True }
                |> Expect.equal { enemyHp = 0, lastEnemyHp = 10, exp = 30, level = 2, point = 0, attack = 6, levelFlag = False }

        ]        
