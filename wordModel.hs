import Data.Map

{-
This is a module for a WordModel, a basic model that is trained to 
predict word probabilities from a given corpus.

There is the possibility of extending this later.
-}

-- WordModel wordCountDict vocabSize
data WordModel = WordModel WordCount Int 
                 | EmptyModel ()

-- Dictionary of from words to their count
type WordCount = Map [Char] Int 

{-
'known wm w' Returns True if the word w is known to the model wm
-}
known :: WordModel -> [Char] -> Bool
known w wm = True

{-
'wordProb wm w' Returns the probability of the occurence of word w
according to the word model wm
-}
wordProb :: Num a => WordModel -> [Char] -> a
wordProb wm w = 1

{-
Train the word model on a corpus of words
-}
train :: WordModel -> [[Char]] -> WordModel
train wm corpus = EmptyModel ()

{-
Increments the count of the given word, ie. increasing its probability
in the model
-}
incrementCount :: WordModel -> [Char] -> WordModel
incrementCount wm w = EmptyModel ()