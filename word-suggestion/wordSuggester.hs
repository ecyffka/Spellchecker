-- Inspired by http://www.norvig.com/spell-correct.html

--module WordSuggester(findSuggestions, findSuggestionsWithProb, editProbLog, candidates) where
module WordSuggester where

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
findSuggestions w wm = map fst (mostProbable wm 5 w (candidates wm w))

{-
'findSuggestions w wm' Returns the 5 most probable suggestions
of words to replace the mispelled word m. Uses the WordModel wm
in probability calculations.
Returns a list of tuples (word, probability)
-}
findSuggestionsWithProb :: [Char] -> WordModel -> [([Char],Double)]
findSuggestionsWithProb w wm = mostProbable wm 5 w (candidates wm w)

{-
'mostProbable wm n candidates' Selects the n most probable words from 
the list of candidate words, according to the WordModel wm
Returns a list of n tuples containing the word and its probability (word,prob)
-}
mostProbable :: WordModel -> Int -> [Char] -> WordEdits -> [([Char],Double)]
mostProbable wm n w candidates = take n $ sortBy (flip (compare `on` snd)) (Map.toList(suggestionProbs wm candidates w))

{-
'probabilities wm m w' 
Generates the log probabilities of a map m of edited words,
given the starting word w
Uses the given word model wm for word probabilities.

Probability is calculated as:
log((word probability)*(edit probability))
- Word probability comes from the WordModel
- See editProbLog for the edit probability
-}
suggestionProbs :: WordModel -> WordEdits -> [Char] -> WordProbs
suggestionProbs wm m w = Map.foldrWithKey 
    (\w2 e m2 -> Map.insert w2 (log (wordProb wm w2) + (editProbLog w e)) m2) 
    Map.empty m

{-
'editProbLog w edits'
Calculate the log probability of the edits being applied to w to
generate a particular word.
Given a list of possible edits: [[e1],[e2],...[en]],
it calculates the log probability as:
log(P(e1)+P(e2)+...+P(en))

where P(ei) = 1/((length w)*(26+26+1+1)*(length ei))
- Why? We multiply the length of w by (26+26+1+1) to find the number of possible
  edits at distance 1 from w. Then, we multiply by the number of edits
  in the sequence of edits ei.

Note that this is an approximation, because when the
edit distance is greater than one, the length of the word
may change after the first edit. For simplicity, we use the
original word length.
-}
oneEditProb n = n*(26+26+1+1)
editProbLog :: [Char] -> [[Char]] -> Double 
editProbLog w e = log (foldr (\e1 res -> res + (1 / fromIntegral (oneEdit * (length e1)))) 0 e) where oneEdit = oneEditProb (length w)

{-
'candidates wm w' Generates a map of all possible words within edit 
distance 2 from the given word w.
Eliminates words that are not known to the WordModel wm
-}
candidates :: WordModel -> [Char] -> WordEdits
candidates wm w = Map.filterWithKey (\k _ -> known wm k) $ edits1set (edits1 (Map.fromList [(w,[])]) w [])

-- Set of operations to apply to a word, tuple of the function and the 
-- shorthand name for the operation
operations :: [([([Char], [Char])] -> [[Char]], Char)]
operations = [(inserts,'i'),(deletes,'d'),(replaces,'r'),(transposes,'t')]

{-
'edits1 m w edits'
Add to the map m all possible words of edit distance 1 from the given word w
Considers insertions, deletions, replacements, and swaps.
edits: edits previously applied to attain w
-}
edits1 :: WordEdits -> [Char] -> [[Char]] -> WordEdits
edits1 m w prevEdits = foldr 
    (\(op,e) m2 -> addWordsToMap m2 (op s) prevEdits e)
    m operations 
    where s = splits "" w

{-
Add to the map all possible words of edit distance 1 from the given map of words
Considers insertions, deletions, replacements, and swaps
-}
edits1set :: WordEdits -> WordEdits
edits1set m = Map.foldrWithKey (\w e m2 -> edits1 m2 w e) Map.empty m

{-
'addWordsToMap m wList edits e'
Given a list of words wList, a list of previously applied edits, and a new edit e,
add the words to the word map m.
If the word is already in the map, add the new edit sequence that generates it.
If not, add it to the map.
-}
addWordsToMap :: WordEdits -> [[Char]] -> [[Char]] -> Char -> WordEdits
addWordsToMap m wList edits e = foldr 
    (\w m2-> Map.insertWith (++) w (if edits == [] then [[e]] else (map (e:) edits)) m2) 
    m wList

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