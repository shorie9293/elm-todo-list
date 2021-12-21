module StatusTests exposing (attackToEnemyTests)

import TodoMain exposing (initModel, reduceEnemyHp)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


attackToEnemyTests : Test
attackToEnemyTests =
    describe "敵に攻撃する"
        [ test "敵のHPを初期値からAttack分減らす" <|
            \_ ->
                reduceEnemyHp initModel
                    |> Expect.equal (initModel.enemyHp - initModel.attack)
        ]        
