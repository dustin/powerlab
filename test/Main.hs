import CRCTest
import StatusTest
import MiniJSONTest

import Test.Framework.Runners.Options
import Test.Framework (defaultMainWithOpts, interpretArgsOrExit)
import System.Environment (getArgs)


main :: IO ()
main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts (CRCTest.tests ++ StatusTest.tests ++ MiniJSONTest.tests)
            opts { ropt_hide_successes = Just True }
