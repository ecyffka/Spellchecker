module Interaction where

import System.IO
import WordModel
import WordSuggester
import Data.Char

-- initialize the model
initializeWm :: IO WordModel.WordModel
initializeWm = do
    file <- readFile "dictionary.txt"
    let wm = train (EmptyModel()) (tokenize file)
    return wm

-- references the code at https://www.cs.ubc.ca/~poole/cs312/2023/haskell/TwentyQs.hs
-- main menu
start :: WordModel.WordModel -> IO WordModel.WordModel
start wm = do
      putStrLn "Choose from the options:\n 1. Try a word\n 2. Add a corpus \n 3. Quit"
      res <- getLine
      if (res `elem` ["1","1.", "a", "A"])
            then do
                  newWm <- getNewWord wm
                  start newWm
      else if (res `elem` ["2","2.", "b", "B"])
            then do
                  newWm <- getNewCorpus wm
                  start newWm
      else do
            putStrLn "Bye!"
            return wm

-- asks the user a word to check
getNewWord :: WordModel.WordModel -> IO WordModel.WordModel
getNewWord wm = do
      putStrLn "What word do you want to check?"
      newWord <- getLine
      let p = findSuggestions newWord wm
      putStr(show p)
      newWm <- askUser wm
      return newWm

-- get a new correct word and train the model
askUser :: WordModel.WordModel -> IO WordModel.WordModel
askUser wm = do
      putStrLn "\nChoose the correct spelling or type in a new correct word."
      correct <- getLine
      let newWm = train wm (tokenize correct)
      putStr "The word model is updated with the given word \""
      putStr correct 
      putStr "\".\n"
      return newWm

-- get a new corpus (string) and train the model
getNewCorpus :: WordModel.WordModel -> IO WordModel.WordModel
getNewCorpus wm = do
      putStrLn "Type in a sentence to be used as a corpus."
      newC <- getLine
      let newWm = train wm (tokenize newC)
      putStr "The word model is updated with the given sentence \""
      putStr newC 
      putStr "\".\n"
      return newWm

go :: IO WordModel
go = do
      initialWm <- initializeWm
      start initialWm