module Test.Generated.Main exposing (main)

import StatusTests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 198683570848598
        , processes = 8
        , globs =
            []
        , paths =
            [ "F:\\Document\\000Program\\elm\\elm-todo-list\\tests\\StatusTests.elm"
            ]
        }
        [ ( "StatusTests"
          , [ Test.Runner.Node.check StatusTests.attackToEnemyTests
            ]
          )
        ]