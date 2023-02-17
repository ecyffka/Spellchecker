module WordModel where

import qualified Data.Map as Map
import Data.IntMap (findWithDefault)
import Data.List.Split
import Data.Char
import Text.Read

{-
This is a module for a WordModel, a basic model that is trained to 
predict word probabilities from a given corpus.

There is the possibility of extending this later.
-}

-- WordModel wordCountDict numTokens
data WordModel = WordModel WordCount Int 
                 | EmptyModel ()

-- Dictionary of from words to their count
type WordCount = Map.Map [Char] Int 

{-
'known wm w' Returns True if the word w is known to the model wm
-}
known :: WordModel -> [Char] -> Bool
known (EmptyModel ()) w = False
known (WordModel m n) w = Map.member w m

{-
'wordProb wm w' Returns the probability of the occurence of word w
according to the word model wm
Uses laplace smoothing to prevent '0' probabilities, except in the 
case of an empty (untrained) model
-}
wordProb :: Fractional a => WordModel -> [Char] -> a
wordProb (EmptyModel ()) w = 0
wordProb (WordModel m n) w = fromIntegral (Map.findWithDefault 0 w m + 1) / fromIntegral (n + Map.size m)

{-
'logProb wm w' Returns the negative log probability of the occurence of word w
according to the word model wm
Uses laplace smoothing to prevent '0' probabilities, except in the 
case of an empty (untrained) model
* Note that if you want to maximize probability, you should minimize 
  negative log of probability
-}
logProb :: Floating a => WordModel -> [Char] -> a
logProb (EmptyModel ()) w = 0
logProb (WordModel m n) w = - (log (fromIntegral (Map.findWithDefault 0 w m + 1)) - log (fromIntegral (n + Map.size m)))

{-
Train the word model on a corpus of words
Note that this will add to the existing counts if the given
WordModel already contains counts
-}
train :: WordModel -> [[Char]] -> WordModel
train (EmptyModel ()) corpus = train (WordModel Map.empty 0) corpus
train wm corpus = foldr (flip incrementCount) wm corpus

{-
Increments the count of the given word, ie. increasing its probability
in the model
-}
incrementCount :: WordModel -> [Char] -> WordModel
incrementCount (EmptyModel ()) w = EmptyModel ()
incrementCount (WordModel m numTokens) w = 
    WordModel 
    (Map.insertWith (+) w 1 m) 
    (numTokens + 1)

{-
Tokenize a plain text corpus into a list of words
Eliminates punctuation and newline characters, as well as numbers
-}
tokenize :: [Char] -> [[Char]]
tokenize text = filter (\w -> Nothing == (readMaybe w :: Maybe Double) && w /= "") (splitOneOf remove (map toLower text))

-- chars to split text on
remove = " \"!@#$%&[](){}_<>-?:;?/'`~.,*+\n"

{-
For testing purposes, show a WordModel by printing
its dict
-}
instance Show WordModel where
    show (WordModel m n) = show n ++ "    " ++ show m