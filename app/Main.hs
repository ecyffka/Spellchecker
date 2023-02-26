module Main where

import qualified Interaction
import Interaction (go)
import qualified WordModel

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

main :: IO WordModel.WordModel
main = go
