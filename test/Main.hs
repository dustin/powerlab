import CRCTest
import StatusTest

import Test.Framework.Runners.Options
import Test.Framework (defaultMainWithOpts, interpretArgsOrExit, testGroup)
import System.Environment (getArgs)


main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts (CRCTest.tests ++ StatusTest.tests)
            opts { ropt_hide_successes = Just True }
