module Interaction where

import System.IO
import WordModel
import WordSuggester
import Data.Char

-- initialize the model
initializeWm :: IO WordModel.WordModel
initializeWm = do
    file <- readFile "word-suggestion/brown_nolines.txt"
    let wm = train (EmptyModel()) (tokenize file)
    return wm

-- references the code at https://www.cs.ubc.ca/~poole/cs312/2023/haskell/TwentyQs.hs
-- main menu
start :: WordModel.WordModel -> IO WordModel.WordModel
start wm = do
      putStrLn "\nChoose from the options:\n 1. Try a word\n 2. Spellcheck a sentence\n 3. Add a corpus \n 4. Quit"
      res <- getLine
      if (res `elem` ["1","1.", "a", "A"])
            then do
                  (newWm, correct) <- getNewWord wm
                  start newWm
      else if (res `elem` ["2","2.", "b", "B"])
            then do
                  newWm <- spellcheck wm
                  start newWm
      else if (res `elem` ["3","3.", "c", "C"])
            then do
                  newWm <- getNewCorpus wm
                  start newWm
      else do
            putStrLn "Bye!"
            return wm

{-
Asks the user for a sentence to spellcheck, then iterates through
the words in the sentence, providing suggestions for misspelled words.
Finally, prints the fixed sentence to the console.
-}
spellcheck :: WordModel.WordModel -> IO WordModel.WordModel
spellcheck wm = do
      putStrLn "Enter the text to spellcheck:"
      sent <- getLine
      let tokens = splitInput sent
      (newWm, fixedTokens) <- checkWords wm tokens
      let fixedSent = reassemble fixedTokens
      putStr "Corrected text: "
      putStrLn fixedSent
      return newWm

{-
'checkWords wm lst'
Checks each word in the list lst of input words against the word model wm. 
If a given word is not the word model, it suggests words to replace
it with.
Output: (wm, fixedTokens) updated word model and corrected tokens 
-}

checkWords :: WordModel -> [[Char]] -> IO (WordModel.WordModel, [[Char]])
checkWords wm [] = do return (wm,[])
checkWords wm (h:t) =
      if ((known wm (map toLower h)) || (tokenize h == [])) then do
            (wm2, tokens) <- checkWords wm t
            return (wm2, h:tokens)
      else do
            putStrLn("The word \""++h++"\" is incorrect.")
            let suggestions = findSuggestions h wm
            putStr(show suggestions)
            (wm2, correct) <- askUser wm
            (wm3, tokens) <- checkWords wm2 t
            return (wm3, correct:tokens)

{-
This splits the user's input into tokens (either actual words or punctuation to
be retained the final output).
Input: lst ([[Char]], the list of words to parse)
Output: [[Char]] the list of tokens contained in the input string
-}
punctuation = " \"!@#$%&[](){}_<>-?:;?/'`~.,*+\n"
splitInput :: [Char] -> [[Char]]
splitInput lst = foldr (\e (h:t) -> 
      if elem e punctuation then []:[e]:h:t
      else (e:h):t) 
      [[]] lst

{-
This puts the correct list of words back together to return to the user.
-}
reassemble :: [[Char]] -> [Char]
reassemble lst =  foldr (++) "" lst

{-
Asks the user for a word to check for misspellings, and
if it is not a known word, suggests correct words to fix the typo.
Output: (wm, correct) the updated word model and the correct word
         chosen by the user
-}
getNewWord :: WordModel.WordModel -> IO (WordModel.WordModel, [Char])
getNewWord wm = do
      putStrLn "What word do you want to check?"
      newWord <- getLine
      let p = findSuggestions (map toLower newWord) wm
      putStr(show p)
      (newWm, correct) <- askUser wm
      return (newWm, correct)

{-
Asks the user which word is correct to fix the typo.
The word model's frequencies are updated based on user input.
Output: (wm, correct) the updated word model and the correct word
         chosen by the user
-}
askUser :: WordModel.WordModel -> IO (WordModel.WordModel, [Char])
askUser wm = do
      putStrLn "\nChoose the correct spelling or type in a new correct word:"
      correct <- getLine
      let newWm = incrementCount wm correct
      putStrLn("The word model is updated with the given word \""++correct++"\".\n")
      return (newWm, correct)

{-
Asks the user for a new sentence to add to the model's
training data.
This updates the existing model, and does not delete previous learning.
-}
getNewCorpus :: WordModel.WordModel -> IO WordModel.WordModel
getNewCorpus wm = do
      putStrLn "Type in a sentence to be added to the corpus."
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