import Test.HUnit
import qualified Data.Map as Map

import WordModelTest

{-
Running tests: use "cabal run test"
-}

tests = TestList[wordModelTests]

-- Entry to run all tests
main = do
    runTestTT tests
