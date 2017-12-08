import CRCTest
import StatusTest
import PowerlabTest

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All Tests" [PowerlabTest.tests,
                                            CRCTest.tests,
                                            StatusTest.tests]
