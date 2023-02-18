-- Inspired by http://www.norvig.com/spell-correct.html

module WordSuggester(findSuggestions, findSuggestionsWithProb) where

import qualified Data.Map as Map
import Data.List
import Data.Function
import WordModel

-- k: word, v: (list of edits to achieve w, probability of w given typo)
type WordEdits = Map.Map [Char] [[Char]]
-- k: word, v: probability of word given typo
type WordProbs = Map.Map [Char] Double

{-
'findSuggestions w wm' Returns the 5 most probable suggestions
of words to replace the mispelled word m. Uses the WordModel wm
in probability calculations.
-}
findSuggestions :: [Char] -> WordModel -> [[Char]]
findSuggestions w wm = map fst (mostProbable wm 5 (candidates wm w))

{-
'findSuggestions w wm' Returns the 5 most probable suggestions
of words to replace the mispelled word m. Uses the WordModel wm
in probability calculations.
Returns a list of tuples (word, probability)
-}
findSuggestionsWithProb :: [Char] -> WordModel -> [([Char],Double)]
findSuggestionsWithProb w wm = mostProbable wm 5 (candidates wm w)

{-
'mostProbable wm n candidates' Selects the n most probable words from 
the list of candidate words, according to the WordModel wm
Returns a list of n tuples containing the word and its probability (word,prob)
-}
mostProbable :: WordModel -> Int -> WordEdits -> [([Char],Double)]
mostProbable wm n candidates = take n $ sortBy (flip (compare `on` snd)) (Map.toList(suggestionProbs wm candidates))

{-
'probabilities wm m' 
Generates the probabilities of a map m of edited words.
Uses the given word model wm for word probabilities.

Probability is calculated as:
(word probability)*(1/(edit distance))
If a word appears multiple times (there is more than one way to generate
the word from the typo), then the probabilities for that word are summed
-}
suggestionProbs :: WordModel -> WordEdits -> WordProbs
suggestionProbs wm m = Map.foldrWithKey 
    (\w e m2 -> Map.insert w ((wordProb wm w)*((editProb e)^2)) m2) 
    Map.empty m

{-
'editProb edits'
Calculate the probability of a word based on the edits
Given a list of possible edits to arrive at the word: [[e1],[e2],...[en]],
it calculates the probability as (1/(length e1))*(1/(length e2))*...*(1/(length en))
-}
editProb :: [[Char]] -> Double 
editProb = foldr (\e1 res -> res / fromIntegral (length e1)) 1

{-
'candidates wm w' Generates a map of all possible words within edit 
distance 2 from the given word w.
Eliminates words that are not known to the WordModel wm
-}
candidates :: WordModel -> [Char] -> WordEdits
candidates wm w = Map.filterWithKey (\k _ -> known wm k) $ edits1set (edits1 (Map.fromList [(w,[])]) w)

-- Set of operations to apply to a word, tuple of the function and the 
-- shorthand name for the operation
operations :: [([([Char], [Char])] -> [[Char]], Char)]
operations = [(inserts,'i'),(deletes,'d'),(replaces,'r'),(transposes,'t')]

{-
'edits1 m w'
Add to the map m all possible words of edit distance 1 from the given word w
Considers insertions, deletions, replacements, and swaps
-}
edits1 :: WordEdits -> [Char] -> WordEdits
edits1 m w = foldr 
    (\(op,e) m2 -> addWordsToMap m2 (op s) prevEdits e)
    m operations 
    where s = splits "" w
          prevEdits = Map.findWithDefault [] w m

{-
Add to the map all possible words of edit distance 1 from the given map of words
Considers insertions, deletions, replacements, and swaps
-}
edits1set :: WordEdits -> WordEdits
edits1set m = Map.foldrWithKey (\w e m2 -> edits1 m2 w) m m

{-
'addWordsToMap m wList edits e'
Given a list of words wList, a list of previously applied edits, and a new edit e,
add the words to the word map m.
If the word is already in the map, add the new edit sequence that generates it.
If not, add it to the map.
-}
addWordsToMap :: WordEdits -> [[Char]] -> [[Char]] -> Char -> WordEdits
addWordsToMap m wList edits e = foldr (\w m2-> Map.insertWith (++) w (map (e:) edits) m2) m wList

{-
'splits prefix w' 
Generate a list of splits for the word w, where each entry in the list is 
a tuple of the string before the split and the string after the split. 
Starts with the given prefix.
eg. splits "" "apple" => [("","apple"),("a","pple"),("ap","ple"),("app","le"),("appl","e"),("apple","")]
-}
splits :: [Char] -> [Char] -> [([Char],[Char])]
splits prefix [] = [(prefix,"")]
splits prefix (h:t) = (prefix,h:t): splits (prefix++[h]) t

{-
'inserts splits'
From the list of splits for the word w, generate a list of possible words that could be generated
from inserting a character into the word w. At every split location, one word will be generated for 
every letter in the alphabet.
-}
inserts :: [([Char],[Char])] -> [[Char]]
inserts = foldr (\(prefix,end) res -> map (\c -> prefix++(c:end)) ['a'..'z'] ++ res) []

{-
From the list of splits for the word w, generate a list of possible words that could be generated
by deleting a character from the word w. At every split location, one character will be deleted.
-}
deletes :: [([Char],[Char])] -> [[Char]]
deletes = foldr (\(prefix,end) res -> if null end then res else (prefix ++ tail end):res) []

{-
From the list of splits for the word w, generate a list of possible words that could be generated
from replacing a character into the word w. At every split location, one word will be generated for 
every letter in the alphabet.
-}
replaces :: [([Char],[Char])] -> [[Char]]
replaces = foldr (\(prefix,end) res -> if null end then res else map (\c -> prefix++(c:tail end)) ['a'..'z'] ++ res) []
{-
From the list of splits for the word w, generate a list of possible words that could be generated
from swapping a character with its neighbour in the word w. At every split location, a word will
be generated by swapping the next two characters
-}
transposes :: [([Char],[Char])] -> [[Char]]
transposes = foldr (\(prefix,end) res -> if length end < 2 then res else let (h1:h2:t) = end in (prefix ++(h2:h1:t)):res) []